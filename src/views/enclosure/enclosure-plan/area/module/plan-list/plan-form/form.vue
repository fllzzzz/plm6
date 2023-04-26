<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    :wrapper-closable="false"
    size="700px"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="120px">
        <el-form-item label="批次命名" prop="name">
          <el-input
            v-model.trim="form.name"
            type="text"
            placeholder="请填写"
            style="width: 270px;"
            maxlength="20"
          />
        </el-form-item>
        <el-form-item label="交货时间" prop="date">
          <el-date-picker
            v-model="form.date"
            type="date"
            value-format="x"
            placeholder="选择交货时间"
            style="width:270px"
            :disabledDate="dateOptionFn"
          />
        </el-form-item>
        <el-form-item label="工程量" prop="quantityWork">
          <el-input-number
            v-model.number="form.quantityWork"
            :min="0"
            :max="99999999"
            :step="1"
            placeholder="请填写"
            controls-position="right"
            style="width: 270px;"
          />
        </el-form-item>
        <el-form-item label="备注" prop="remark">
          <el-input
            v-model.trim="form.remark"
            type="textarea"
            show-word-limit
            :autosize="{ minRows: 4, maxRows: 6}"
            maxlength="200"
            placeholder="请填写备注"
            style="width: 320px;"
          />
        </el-form-item>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, defineProps } from 'vue'
import { regForm } from '@compos/use-crud'

const props = defineProps({
  detailInfo: {
    type: Object,
    default: () => {}
  }
})

const formRef = ref()
const defaultForm = {
  id: undefined,
  monomerId: undefined,
  name: '',
  quantityWork: undefined,
  date: undefined,
  remark: ''
}

const { CRUD, crud, form } = regForm(defaultForm, formRef)

const checkOtherDate = (rule, value, callback) => {
  if (!value) {
    callback(new Error('请选择交货时间'))
  }
  callback()
}
const rules = {
  date: [
    { required: true, validator: checkOtherDate, trigger: 'change' }
  ],
  quantityWork: [
    { required: true, message: '请填写工程量', trigger: 'change', type: 'number' }
  ],
  name: [
    { required: true, message: '请填写批次命名', trigger: 'blur' },
    { min: 1, max: 20, message: '长度在 1 到 20 个字符', trigger: 'blur' }
  ],
  remark: [{ max: 500, message: '不能超过 500 个字符', trigger: 'blur' }]
}

function dateOptionFn(time) {
  // return time.getTime() < new Date().getTime()
  return time.getTime() < props.detailInfo.startDate || time.getTime() > props.detailInfo.endDate
}

CRUD.HOOK.beforeSubmit = (crud, form) => {
  crud.form.techId = crud.query.techId
  return !!crud.form.techId
}
</script>
<style scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>
