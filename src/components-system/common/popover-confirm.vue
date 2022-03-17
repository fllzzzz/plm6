<template>
  <el-popover placement="top-start" width="450" v-model:visible="visible">
    <template #reference>
      <slot name="reference" />
    </template>
    <template #default>
      <slot name="default" />
      <div style="text-align: right; margin-top: 10px">
        <common-button size="mini" type="text" @click="cancel"> {{ props.cancelButtonText }} </common-button>
        <common-button size="mini" type="primary" @click="confirm"> {{ props.confirmButtonText }} </common-button>
      </div>
    </template>
  </el-popover>
</template>

<script setup>
import { ref, defineProps, defineEmits, watch } from 'vue'

const emit = defineEmits(['confirm', 'cancel', 'visible'])

const props = defineProps({
  confirmButtonText: {
    type: String,
    default: '确认'
  },
  cancelButtonText: {
    type: String,
    default: '取消'
  }
})

const visible = ref(false)

watch(visible, (newVal) => {
  if (newVal) {
    emit('visible')
  }
})

// 确认
function confirm() {
  visible.value = false
  emit('confirm')
}

// 取消
function cancel() {
  visible.value = false
  emit('cancel')
}
</script>
