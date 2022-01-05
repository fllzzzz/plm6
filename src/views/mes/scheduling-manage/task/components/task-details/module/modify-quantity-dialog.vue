<template>
  <common-dialog
    append-to-body
    title="排产数量修改"
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
        :disabled="modifySchedulingQuantity === details.sourceSchedulingQuantity"
        @click="submitIt"
        >确认</common-button
      >
    </template>
    <el-form ref="formRef" :model="details" :rules="rules" size="small" label-width="100px">
      <el-form-item label="生产线">
        <span>{{ emptyTextFormatter(details.workshop?.name) }}>{{ emptyTextFormatter(details.productionLine?.name) }}</span>
      </el-form-item>
      <el-form-item label="所属项目">
        <span>{{ emptyTextFormatter(details.project?.name) }}</span>
      </el-form-item>
      <el-form-item label="单体区域">
        <span>{{ emptyTextFormatter(details.monomer?.name) }}>{{ emptyTextFormatter(details.area?.name) }}</span>
      </el-form-item>
      <el-form-item label="编号">
        <span>{{ emptyTextFormatter(details.productSerialNumber) }}</span>
      </el-form-item>
      <el-form-item label="排产数" prop="modifySchedulingQuantity">
        <el-input-number
          v-model="modifySchedulingQuantity"
          :step="1"
          :min="0"
          :max="details.sourceSchedulingQuantity"
          size="small"
          controls-position="right"
          style="width: 250px"
        />
      </el-form-item>
      <el-form-item label="修改状态">
        <template v-if="modifySchedulingQuantity === details.sourceSchedulingQuantity">
          <span>暂未修改</span>
        </template>
        <template v-else>
          <span>{{ details.sourceSchedulingQuantity }}</span>
          <span
v-if="modifySchedulingQuantity !== details.sourceSchedulingQuantity"
            >▶<span :style="{ color: modifySchedulingQuantity < details.sourceSchedulingQuantity ? '#11b95c' : 'red' }">{{
              modifySchedulingQuantity
            }}</span></span
          >
        </template>
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { modifyQuantity } from '@/api/mes/scheduling-manage/task/common'
import { defineProps, ref, watch, defineEmits } from 'vue'
import { ElNotification } from 'element-plus'

import { emptyTextFormatter } from '@data-type'

import useVisible from '@compos/use-visible'

const rules = {
  modifySchedulingQuantity: [{ required: true, message: '请填写任务数', trigger: 'blur' }]
}
const formRef = ref()
const emit = defineEmits(['modifySuccess', 'update:visible'])
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
const modifySchedulingQuantity = ref()

watch(
  () => props.visible,
  (visible) => {
    if (visible) {
      modifySchedulingQuantity.value = props.details.modifySchedulingQuantity
    }
  },
  { immediate: true }
)

function submitIt() {
  formRef.value.validate(async (valid) => {
    if (valid) {
      const data = {
        id: props.details.id,
        schedulingQuantity: modifySchedulingQuantity.value
      }
      await modifyQuantity(data)
      ElNotification({ title: '排产数量修改成功', type: 'success', duration: 2500 })
      emit('modifySuccess')
      handleClose()
    } else {
      return false
    }
  })
}
</script>
