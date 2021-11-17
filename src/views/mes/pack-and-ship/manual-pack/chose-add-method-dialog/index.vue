<template>
  <common-dialog title="选择新增类型" v-model="dialogVisible" :before-close="handleClose" width="450px">
    <template #titleRight>
      <common-button :loading="packLoading" type="primary" size="mini" @click="confirmIt">确 定</common-button>
    </template>
    <el-radio-group v-model="isNew">
      <el-radio :label="true">生成新包单</el-radio>
      <el-radio :label="false">使用原有包单</el-radio>
    </el-radio-group>
    <div v-show="!isNew" style="margin-top: 15px">
      <pack-select v-model="selectBagId" :project-id="projectId" :packType="packType" style="width:100%;"/>
    </div>
  </common-dialog>
</template>

<script setup>
import { defineProps, defineEmits, defineExpose, ref, inject } from 'vue'

import useVisible from '@compos/use-visible'
import packSelect from '@comp-mes/pack-select'

const emit = defineEmits(['update:visible', 'handlePack'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  packLoading: {
    type: Boolean,
    default: false
  },
  packType: {
    type: Number,
    default: undefined
  }
})
const projectId = inject('projectId')
const isNew = ref(true)
const selectBagId = ref()

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible' })

function handleSuccess() {
  isNew.value = true
  selectBagId.value = undefined
  handleClose()
}

function confirmIt() {
  emit('handlePack', { isNew: isNew.value, selectBagId: selectBagId.value })
}

defineExpose({
  handleSuccess
})
</script>
