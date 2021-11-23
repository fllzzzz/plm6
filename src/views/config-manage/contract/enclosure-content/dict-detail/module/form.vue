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
      <el-form-item label="字典标签" prop="label">
        <el-input
          v-model="form.label"
          placeholder="输入字典标签"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
        />
      </el-form-item>
      <el-form-item label="排序" prop="sort">
        <el-input-number
          class="align-left"
          v-model="form.sort"
          placeholder="请填写"
          type="text"
          controls-position="right"
          style="width: 200px"
          :min="0"
        />
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { ref } from 'vue'
import { regForm } from '@compos/use-crud'

const formRef = ref()

const defaultForm = {
  id: undefined,
  label: undefined,
  sort: undefined,
  name: '',
  type: undefined
}

const { crud, form } = regForm(defaultForm, formRef)

const rules = {
  label: [{ required: true, message: '请输入字典标签', trigger: 'blur' }],
  sort: [{ required: true, message: '请输入排序', trigger: 'blur', type: 'number' }]
}

</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>
