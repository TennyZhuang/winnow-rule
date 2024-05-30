use proc_macro2::TokenTree;
use winnow::stream::{Checkpoint, Offset, SliceLen, Stream, StreamIsPartial, UpdateSlice};

#[derive(Copy, Clone, Debug)]
pub struct CheckpointWrapper<'a>(Checkpoint<&'a [TokenTree], &'a [TokenTree]>);

impl<'a> Offset<CheckpointWrapper<'a>> for CheckpointWrapper<'a> {
    #[inline(always)]
    fn offset_from(&self, start: &Self) -> usize {
        self.0.offset_from(&start.0)
    }
}

/// Customized wrapper that implements [`Stream`] override [`Debug`] for better diagnostics.
#[derive(Copy, Clone)]
pub struct InputWrapper<'a>(pub &'a [TokenTree]);

impl<'a> std::fmt::Debug for InputWrapper<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for tt in self.0 {
            write!(f, "{}", tt)?;
        }
        Ok(())
    }
}

impl<'a> Offset<InputWrapper<'a>> for InputWrapper<'a> {
    #[inline(always)]
    fn offset_from(&self, start: &Self) -> usize {
        self.0.offset_from(&start.0)
    }
}

impl<'a> Offset<CheckpointWrapper<'a>> for InputWrapper<'a> {
    #[inline(always)]
    fn offset_from(&self, start: &CheckpointWrapper<'a>) -> usize {
        self.0.offset_from(&start.0)
    }
}

impl<'a> SliceLen for InputWrapper<'a> {
    #[inline(always)]
    fn slice_len(&self) -> usize {
        self.0.slice_len()
    }
}

impl<'a> StreamIsPartial for InputWrapper<'a> {
    type PartialState = <&'a [TokenTree] as StreamIsPartial>::PartialState;

    #[must_use]
    #[inline(always)]
    fn complete(&mut self) -> Self::PartialState {
        self.0.complete()
    }

    #[inline(always)]
    fn restore_partial(&mut self, state: Self::PartialState) {
        self.0.restore_partial(state)
    }

    #[inline(always)]
    fn is_partial_supported() -> bool {
        <&'a [TokenTree] as StreamIsPartial>::is_partial_supported()
    }
}

impl<'a> Stream for InputWrapper<'a> {
    type Token = <&'a [TokenTree] as Stream>::Token;
    type Slice = InputWrapper<'a>;
    type IterOffsets = <&'a [TokenTree] as Stream>::IterOffsets;
    type Checkpoint = CheckpointWrapper<'a>;

    #[inline(always)]
    fn iter_offsets(&self) -> Self::IterOffsets {
        self.0.iter_offsets()
    }

    #[inline(always)]
    fn eof_offset(&self) -> usize {
        self.0.eof_offset()
    }

    #[inline(always)]
    fn next_token(&mut self) -> Option<Self::Token> {
        self.0.next_token()
    }

    #[inline(always)]
    fn offset_for<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Token) -> bool,
    {
        self.0.offset_for(predicate)
    }

    #[inline(always)]
    fn offset_at(&self, tokens: usize) -> Result<usize, winnow::error::Needed> {
        self.0.offset_at(tokens)
    }

    #[inline(always)]
    fn next_slice(&mut self, offset: usize) -> Self::Slice {
        InputWrapper(self.0.next_slice(offset))
    }

    #[inline(always)]
    fn checkpoint(&self) -> Self::Checkpoint {
        CheckpointWrapper(self.0.checkpoint())
    }

    #[inline(always)]
    fn reset(&mut self, checkpoint: &Self::Checkpoint) {
        self.0.reset(&checkpoint.0)
    }

    #[inline(always)]
    fn raw(&self) -> &dyn std::fmt::Debug {
        // We customized the `Debug` implementation in the wrapper, so don't use `self.0` here.
        self
    }
}

impl<'a> UpdateSlice for InputWrapper<'a> {
    #[inline(always)]
    fn update_slice(self, inner: Self::Slice) -> Self {
        InputWrapper(self.0.update_slice(inner.0))
    }
}
