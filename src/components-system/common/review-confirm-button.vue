<template>
  <el-popconfirm
    confirm-button-text="确定"
    cancel-button-text="取消"
    icon="el-icon-info"
    icon-color="green"
    title="确认通过？"
    @confirm="passed"
  >
    <template #reference>
      <common-button :loading="disableOperate" type="primary" size="mini" icon="el-icon-s-promotion"> 通 过 </common-button>
    </template>
  </el-popconfirm>
  <el-popconfirm
    confirm-button-text="确定"
    cancel-button-text="取消"
    icon="el-icon-info"
    icon-color="red"
    title="确认退回？"
    @confirm="returned"
  >
    <template #reference>
      <common-button :loading="disableOperate" size="mini" icon="el-icon-document-delete" type="danger"> 退 回 </common-button>
    </template>
  </el-popconfirm>
</template>

<script setup>
import { computed, defineProps, defineEmits } from 'vue'

const emit = defineEmits(['update:passedLoading', 'update:returnedLoading'])

const props = defineProps({
  passedFn: {
    type: Function,
    required: true
  },
  returnedFn: {
    type: Function,
    required: true
  },
  passedLoading: {
    type: Boolean,
    default: false
  },
  returnedLoading: {
    type: Boolean,
    default: false
  }
})

const disableOperate = computed(() => props.passedLoading || props.returnedLoading)

function passed() {
  emit('update:passedLoading', true)
  if (typeof props.passedFn === 'function') {
    props.passedFn()
  }
}

function returned() {
  emit('update:returnedLoading', true)
  if (typeof props.returnedFn === 'function') {
    props.returnedFn()
  }
}
</script>
