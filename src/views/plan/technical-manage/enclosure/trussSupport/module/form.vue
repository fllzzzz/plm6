<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    :wrapper-closable="false"
    size="500"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="120px">
        <el-form-item label="名称" prop="name">
          <span>{{ form.name }}</span>
        </el-form-item>
        <el-form-item label="编号" prop="serialNumber">
          <span>{{ form.serialNumber }}</span>
        </el-form-item>
        <el-form-item label="版型" prop="plate">
          <el-input v-model="form.plate" type="text" placeholder="请填写版型" style="width: 270px" />
        </el-form-item>
        <el-form-item label="有效宽度" prop="width">
          <el-input-number
            v-model.number="form.width"
            :min="0"
            :max="maxNubmer"
            :step="1"
            :precision="DP.MES_ENCLOSURE_W__MM"
            placeholder="请填写有效宽度"
            controls-position="right"
            style="width: 270px"
          />
        </el-form-item>
        <el-form-item label="长度(mm)" prop="length">
          <el-input-number
            v-model.number="form.length"
            :min="0"
            :max="maxNubmer"
            :step="1"
            :precision="DP.MES_ENCLOSURE_L__MM"
            placeholder="请填写长度"
            controls-position="right"
            style="width: 270px"
          />
        </el-form-item>
        <el-form-item label="数量" prop="quantity">
          <el-input-number
            v-model.number="form.quantity"
            :min="0"
            :max="maxNubmer"
            :step="1"
            placeholder="请填写数量"
            controls-position="right"
            style="width: 270px"
          />
        </el-form-item>
        <el-form-item label="备注" prop="remark">
          <el-input
            v-model.trim="form.remark"
            type="textarea"
            :autosize="{ minRows: 1, maxRows: 6 }"
            placeholder="请填写备注"
            style="width: 320px"
          />
        </el-form-item>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, defineProps, watch, computed } from 'vue'
import { regForm } from '@compos/use-crud'
import IconSelect from '@comp/iconSelect/index.vue'
import { isNotBlank } from '@data-type/index'
import { DP } from '@/settings/config'

const formRef = ref()
const isdisable = ref(false)
const maxNubmer = 999999999
const defaultForm = {
  id: undefined,
  name: '',
  serialNumber: '',
  plate: '',
  width: undefined,
  length: undefined,
  quantity: '',
  remark: '',
}
const { CRUD, crud, form } = regForm(defaultForm, formRef)

const rules = {
  name: [
    { required: true, message: '请填写名称', trigger: 'blur' },
    { min: 1, max: 64, message: '长度在 1 到 64 个字符', trigger: 'blur' },
  ],
  serialNumber: [
    { required: true, message: '请填写编号', trigger: 'blur' },
    { min: 1, max: 64, message: '长度在 1 到 64 个字符', trigger: 'blur' },
  ],
  plate: [
    { required: true, message: '请填写编号', trigger: 'blur' },
    { min: 1, max: 64, message: '长度在 1 到 64 个字符', trigger: 'blur' },
  ],
  width: [{ required: true, message: '请填写有效宽度', trigger: 'blur', type: 'number' }],
  length: [{ required: true, message: '请填写长度', trigger: 'blur', type: 'number' }],
  quantity: [{ required: true, message: '请填写数量', trigger: 'blur', type: 'number' }],
  remark: [{ max: 500, message: '不能超过 500 个字符', trigger: 'blur' }],
}

CRUD.HOOK.beforeSubmit = (crud, form) => {
  crud.form.totalArea = (crud.form.width * crud.form.length * crud.form.quantity) / 1000000
  crud.form.totalLength = (crud.form.length * crud.form.quantity) / 1000
}
</script>
<style rel="stylesheet/scss" lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
::v-deep(.el-dialog__body) {
  padding: 10px 20px;

  .el-step {
    .el-step__icon {
      width: 20px;
      height: 20px;
      font-size: 12px;
    }
    .el-step__title {
      font-size: 13px;
    }
  }
}
.tree-form {
  ::v-deep(.el-drawer__header) {
    margin-bottom: 0;
  }
}
.item-name {
  padding: 8px 16px;
  background-color: #ecf8ff;
  border-radius: 4px;
  border-left: 5px solid #50bfff;
  margin: 10px 0;
  margin-left: 5px;
  width: 150px;
}
.table-form {
  ::v-deep(.el-input__inner) {
    padding: 0;
    padding-left: 5px;
  }
}
</style>

