<template>
  <common-dialog
    append-to-body
    title="任务删除"
    :close-on-click-modal="false"
    :before-close="handleClose"
    :visible="dialogVisible"
    width="500px"
  >
    <template #titleRight>
      <common-button
        :loading="submitLoading"
        size="mini"
        type="primary"
        :disabled="!form.quantity"
        @click="submitIt"
        >确认</common-button
      >
    </template>
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="100px">
      <el-form-item label="所属项目">
        <span v-empty-text>{{ details.projectName }}</span>
      </el-form-item>
      <el-form-item label="编号">
        <span v-empty-text>{{ details.productSerialNumber }}</span>
      </el-form-item>
      <el-form-item label="任务数">
        <span v-empty-text>{{ details.sourceSchedulingQuantity }}</span>
      </el-form-item>
      <el-form-item label="未完成数">
        <span v-empty-text>{{ unCompleteQuantity }}</span>
      </el-form-item>
      <el-form-item label="删除数量" prop="quantity">
        <el-input-number
          v-model="form.quantity"
          :step="1"
          :min="0"
          :max="unCompleteQuantity"
          size="small"
          controls-position="right"
          style="width: 250px"
        />
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { delTask } from '@/api/mes/scheduling-manage/task/common'
import { defineProps, ref, defineEmits, computed, reactive } from 'vue'
import { ElNotification } from 'element-plus'

import useVisible from '@compos/use-visible'

const rules = {
  quantity: [{ required: true, message: '请填写删除数量', trigger: 'blur' }]
}
const formRef = ref()
const emit = defineEmits(['delSuccess', 'update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    required: true
  },
  details: {
    type: Object,
    default: () => {}
  }
})

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible' })
const submitLoading = ref(false)
const form = reactive({ quantity: null })

const unCompleteQuantity = computed(() => {
  console.log(props.details?.sourceSchedulingQuantity, props.details?.completeQuantity)
  return (props.details?.sourceSchedulingQuantity || 0) - (props.details?.completeQuantity || 0)
})

// watch(
//   () => props.visible,
//   (visible) => {
//     if (visible) {
//       quantity.value = props.details.quantity
//     }
//   },
//   { immediate: true }
// )

function submitIt() {
  formRef.value.validate(async (valid) => {
    if (valid) {
      const data = {
        id: props.details.id,
        quantity: form?.quantity
      }
      await delTask(data)
      ElNotification({ title: '任务删除成功', type: 'success', duration: 2500 })
      emit('delSuccess')
      handleClose()
    } else {
      return false
    }
  })
}
</script>
