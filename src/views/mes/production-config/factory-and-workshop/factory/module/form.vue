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
      <el-form-item label="工厂名称" prop="name">
        <el-input
          v-model.trim="form.name"
          type="text"
          placeholder="请填写名称"
          style="width: 270px;"
        />
      </el-form-item>
      <el-form-item label="工厂简称" prop="shortName">
        <el-input
          v-model.trim="form.shortName"
          type="text"
          placeholder="请填写简称"
          style="width: 270px;"
        />
      </el-form-item>
      <el-form-item label="标签颜色" prop="tagColor">
        <el-color-picker
          v-model="form.tagColor"
          show-alpha
          :predefine="predefineColors"
        />
      </el-form-item>
      <el-form-item label="排序" prop="sort">
        <el-input-number
          v-model.number="form.sort"
          :min="1"
          :max="999"
          :step="1"
          controls-position="right"
          style="width: 270px;"
        />
      </el-form-item>
      <el-form-item label="备注" prop="remark">
        <el-input
          v-model.trim="form.remark"
          type="textarea"
          :autosize="{ minRows: 4, maxRows: 6}"
          placeholder="请填写备注"
          style="width: 320px;"
        />
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { ref } from 'vue'
import { regForm } from '@compos/use-crud'
import { TAG_FACTORY_DEF_COLOR } from '@/config/common'

const formRef = ref()

const defaultForm = {
  id: undefined,
  name: '',
  shortName: '',
  sort: 1,
  remark: '',
  tagColor: TAG_FACTORY_DEF_COLOR
}

const { crud, form } = regForm(defaultForm, formRef)

const predefineColors = [
  '#ff4500',
  '#ff8c00',
  '#ffd700',
  '#90ee90',
  '#00ced1',
  '#1e90ff',
  '#c71585',
  'rgba(255, 69, 0, 0.68)',
  'rgb(255, 120, 0)',
  'hsv(51, 100, 98)',
  'hsva(120, 40, 94, 0.5)',
  'hsl(181, 100%, 37%)',
  'hsla(209, 100%, 56%, 0.73)',
  '#c7158577'
]

const rules = {
  sort: [
    { required: true, message: '请填写排序值', trigger: 'blur', type: 'number' }
  ],
  name: [
    { required: true, message: '请填写工厂名称', trigger: 'blur' },
    { min: 1, max: 32, message: '长度在 1 到 32 个字符', trigger: 'blur' }
  ],
  shortName: [
    { required: true, message: '请填写工厂简称', trigger: 'blur' },
    { min: 1, max: 32, message: '长度在 1 到 32 个字符', trigger: 'blur' }
  ],
  remark: [{ max: 500, message: '不能超过 500 个字符', trigger: 'blur' }]
}

</script>

<style rel="stylesheet/scss" lang="scss" scoped>
  :v-deep .el-input-number .el-input__inner {
    text-align: left;
  }
</style>
