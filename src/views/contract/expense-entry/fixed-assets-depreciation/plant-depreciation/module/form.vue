<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    :show-close="false"
    width="500px"
    top="10vh"
  >
    <template #titleRight>
      <span style="float: right">
        <common-button :loading="crud.status.cu === CRUD.STATUS.PROCESSING" size="mini" type="primary" @click="crud.submitCU">
          提 交
        </common-button>
        <common-button size="mini" @click="crud.cancelCU">关 闭</common-button>
      </span>
    </template>
    <div class="form">
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="140px" class="demo-form">
        <el-form-item label="名称：" prop="name">
          <el-input ref="saveTagInput" v-model="form.name" placeholder="输入名称" style="width: 270px" maxlength="50" />
        </el-form-item>
        <el-form-item label="原值（元）：" prop="originalValue">
          <el-input-number
            v-model="form.originalValue"
            style="width: 270px"
            placeholder="输入原值 单位：元"
            :controls="false"
            :precision="decimalPrecision.contract"
            :min="0"
            :max="9999999999"
          />
        </el-form-item>
        <el-form-item label="折旧年限：" prop="depreciationYear">
          <el-input-number
            v-model="form.depreciationYear"
            style="width: 270px"
            placeholder="输入折旧年限"
            :controls="false"
            :precision="1"
            :step="0.1"
            :min="0"
            :max="1000"
          />
        </el-form-item>
        <el-form-item label="净残值率（%）：" prop="residualValueRate">
          <el-input
            v-model="form.residualValueRate"
            style="width: 270px"
            placeholder="输入净残值率"
            type="number"
            :precision="2"
            maxlength="4"
            :min="0"
            :max="100"
          />
        </el-form-item>
        <el-form-item label="折旧日期：" prop="startDate">
          <el-date-picker
            v-model="form.startDate"
            type="date"
            size="small"
            format="YYYY-MM"
            value-format="x"
            placeholder="选择日期"
            style="width: 270px"
          />
        </el-form-item>
        <el-form-item label="年折旧率(%)：" prop="annualDepreciationRate">
          <span>{{ annualDepreciationRate }}</span>
        </el-form-item>
        <el-form-item label="年折旧额（元）：" prop="annualDepreciationAmount">
          <span>{{ annualDepreciationAmount }}</span>
        </el-form-item>
        <el-form-item label="月折旧率(%)：" prop="monthValueDepreciationRate">
          <span>{{ monthValueDepreciationRate }}</span>
        </el-form-item>
        <el-form-item label="月折旧额（元）：" prop="monthValueDepreciationAmount">
          <span>{{ monthValueDepreciationAmount }}</span>
        </el-form-item>
      </el-form>
    </div>
  </common-dialog>
</template>

<script setup>
import { ref, computed } from 'vue'
import { regForm } from '@compos/use-crud'
import { depreciationTypeEnum } from '@enum-ms/contract'

import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()

const formRef = ref()

const defaultForm = {
  name: undefined,
  originalValue: undefined,
  depreciationYear: undefined,
  startDate: new Date().getTime(),
  residualValueRate: undefined,
  type: depreciationTypeEnum.PLANT.V
}

const { crud, form, CRUD } = regForm(defaultForm, formRef)

const validateQuantity = (rule, value, callback) => {
  if (!value) {
    callback(new Error('填写数据必须大于0'))
  }
  callback()
}

const validateRateQuantity = (rule, value, callback) => {
  if (!value) {
    callback(new Error('填写数据必须大于0'))
  }
  if (value > 100) {
    callback(new Error('填写数据必须小于100'))
  }
  callback()
}

const rules = {
  name: [{ required: true, message: '请输入名称', trigger: 'blur' }],
  originalValue: [{ required: true, validator: validateQuantity, trigger: 'blur' }],
  depreciationYear: [{ required: true, validator: validateQuantity, trigger: 'blur' }],
  residualValueRate: [{ required: true, validator: validateRateQuantity, trigger: 'blur' }],
  startDate: [{ required: true, message: '请选择折旧日期', trigger: 'blur' }]
}

const annualDepreciationRate = computed(() => {
  // （（1-净残值）/ 使用年限）
  return form.residualValueRate && form.depreciationYear
    ? (((100 - form.residualValueRate) / 100 / form.depreciationYear) * 100).toFixed(2)
    : ''
})
const annualDepreciationAmount = computed(() => {
  // 原值*（（1-净残值）/ 使用年限）
  return form.originalValue && form.residualValueRate && form.depreciationYear
    ? (form.originalValue * ((100 - form.residualValueRate) / 100 / form.depreciationYear)).toFixed(decimalPrecision.value.contract)
    : ''
})
const monthValueDepreciationRate = computed(() => {
  // （（1-净残值）/ 使用年限）/ 12
  console.log(form.residualValueRate, 'form.residualValueRate')
  return form.residualValueRate && form.depreciationYear
    ? (((100 - form.residualValueRate) / 100 / form.depreciationYear / 12) * 100).toFixed(2)
    : ''
})
const monthValueDepreciationAmount = computed(() => {
  // 原值*（（1-净残值）/ 使用年限 / 12）
  return form.originalValue && form.residualValueRate && form.depreciationYear
    ? (form.originalValue * ((100 - form.residualValueRate) / 100 / form.depreciationYear / 12)).toFixed(decimalPrecision.value.contract)
    : ''
})

// 编辑之前
CRUD.HOOK.beforeToEdit = (crud, form) => {
  form.residualValueRate = (form.residualValueRate * 100)?.toFixed(2)
}

// 新增之前
CRUD.HOOK.beforeToAdd = (crud, form) => {
  form.originalValue = undefined
  form.name = undefined
}

// 处理刷新数据
CRUD.HOOK.beforeToQuery = async () => {}

// 编辑之后
CRUD.HOOK.afterToEdit = (crud, form) => {}

// 提交前
CRUD.HOOK.beforeSubmit = async () => {
  const valid = await formRef.value.validate()
  if (!valid) return false
  form.residualValueRate = (form.residualValueRate / 100)?.toFixed(4)
}
</script>

<style lang="scss" scoped>
:deep(.el-input--small .el-input__inner) {
  text-align: center;
}

:deep(.el-input__inner) {
  line-height: 1px !important;
}
</style>
