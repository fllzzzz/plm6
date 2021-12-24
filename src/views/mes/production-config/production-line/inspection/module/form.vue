<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    width="500px"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="90px">
      <el-form-item label="工序" prop="processId">
        <process-select v-model="form.processId" :productType="productType" containsMachinePart :multiple="false" style="width: 270px" />
      </el-form-item>
      <el-form-item label="质检" prop="inspectorIds">
        <user-select
          ref="inspectorSelectRef"
          v-model="form.inspectorIds"
          :multiple="true"
          placeholder="请选择质检人员"
          style="width: 270px"
          @change="inspectorChange"
        />
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { ref, defineProps } from 'vue'
import { regForm } from '@compos/use-crud'
import processSelect from '@comp-mes/process-select'
import userSelect from '@comp-common/user-select'

defineProps({
  productType: {
    type: Number,
    default: undefined
  }
})

const formRef = ref()
const inspectorSelectRef = ref()

const defaultForm = {
  id: undefined,
  processId: undefined,
  inspectorIds: []
}

const { crud, form } = regForm(defaultForm, formRef)

const rules = {
  processId: [{ required: true, message: '请选择工序', trigger: 'change' }],
  inspectorIds: [{ required: true, message: '请选择质检', trigger: 'change' }]
}

function inspectorChange(userlist) {
  form.inspectors = userlist
}
</script>
