<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    width="470px"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <el-form ref="formRef" :model="form" :rules="rules" size="small">
      <el-form-item>
        <common-radio-button
          v-model="basicClass"
          :options="rawMatClsEnum.ENUM"
          showOptionAll
          type="enum"
          size="small"
          class="filter-item"
        />
      </el-form-item>
      <el-form-item prop="ids">
        <material-cascader
          v-model="form.ids"
          :basic-class="basicClass"
          :disabledVal="disabledIds"
          multiple
          clearable
          size="small"
          style="width: 332px"
          placeholder="请选择需要质检的科目"
        />
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { ref, defineProps } from 'vue'

import { rawMatClsEnum } from '@enum-ms/classification'
import MaterialCascader from '@comp-cls/material-cascader/index.vue'

import { regForm } from '@compos/use-crud'

defineProps({
  disabledIds: {
    type: Array,
    default: () => []
  }
})

const formRef = ref()
const defaultForm = {
  ids: undefined
}
const basicClass = ref()

const { crud, form } = regForm(defaultForm, formRef)

const rules = {
  ids: [{ required: true, message: '请选择需要质检的科目', trigger: 'change' }]
}
</script>
