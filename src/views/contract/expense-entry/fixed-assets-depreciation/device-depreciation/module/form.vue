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
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="160px" class="demo-form">
        <el-form-item label="设备名称" prop="name">
          <el-input v-model="form.name" placeholder="输入设备名称" style="width: 270px" maxlength="50" />
        </el-form-item>
        <el-form-item label="数量（台）" prop="num">
          <el-input-number
            v-model="form.num"
            style="width: 270px"
            placeholder="输入数量"
            controls-position="right"
            :precision="0"
            :min="1"
            :max="9999999999"
          />
        </el-form-item>
        <el-form-item label="单台初始价值（元）" prop="originalValue">
          <el-input-number
            v-show-thousand
            v-model="form.originalValue"
            style="width: 270px"
            placeholder="输入单台初始价值 单位元"
            controls-position="right"
            :min="0"
            :max="9999999999"
          />
        </el-form-item>
        <el-form-item label="折旧年限" prop="depreciationYear">
          <el-input-number
            v-model="form.depreciationYear"
            style="width: 270px"
            placeholder="输入折旧年限"
            controls-position="right"
            :precision="1"
            :step="0.1"
            :min="0"
            :max="1000"
            @change="yearChange"
          />
        </el-form-item>
        <el-form-item label="净残值率（%）" prop="residualValueRate">
          <el-input-number
            v-model="form.residualValueRate"
            style="width: 270px"
            placeholder="输入净残值率"
            controls-position="right"
            :precision="2"
            :min="0"
            :max="100"
          />
        </el-form-item>
        <el-form-item label="折旧开始日期" prop="startDate">
          <el-date-picker
            v-model="form.startDate"
            type="date"
            size="small"
            format="YYYY-MM-DD"
            value-format="x"
            :clearable="false"
            placeholder="选择日期"
            style="width: 270px"
          />
        </el-form-item>
        <el-form-item label="折旧结束日期" prop="endDate">
          <el-date-picker
            v-model="form.endDate"
            :default-time="[new Date(2000, 2, 1, 23, 59, 59)]"
            type="date"
            size="small"
            format="YYYY-MM-DD"
            value-format="x"
            :clearable="false"
            :disabled-date="(time) => form.startDate >= time"
            placeholder="选择折旧结束日期"
            style="width: 270px"
            @change="endChange"
          />
        </el-form-item>
        <el-form-item label="年折旧率(%)" prop="annualDepreciationRate">
          <span v-if="isBlank(annualDepreciationRate)" class="hint">根据上方填写内容自动计算</span>
          <span>{{ annualDepreciationRate }}</span>
        </el-form-item>
        <el-form-item label="年折旧额（元）" prop="annualDepreciationAmount">
          <span v-if="isBlank(annualDepreciationAmount)" class="hint">根据上方填写内容自动计算</span>
          <span>{{ annualDepreciationAmount }}</span>
        </el-form-item>
        <el-form-item label="月折旧率(%)" prop="monthValueDepreciationRate">
          <span v-if="isBlank(monthValueDepreciationRate)" class="hint">根据上方填写内容自动计算</span>
          <span>{{ monthValueDepreciationRate }}</span>
        </el-form-item>
        <el-form-item label="月折旧额（元）" prop="monthValueDepreciationAmount">
          <span v-if="isBlank(monthValueDepreciationAmount)" class="hint">根据上方填写内容自动计算</span>
          <span>{{ monthValueDepreciationAmount }}</span>
        </el-form-item>
      </el-form>
    </div>
  </common-dialog>
</template>

<script setup>
import { ref, computed } from 'vue'

import { isBlank, toFixed } from '@/utils/data-type'

import { regForm } from '@compos/use-crud'
import moment from 'moment'

const formRef = ref()

const defaultForm = {
  id: undefined,
  name: undefined,
  num: undefined,
  originalValue: undefined,
  depreciationYear: undefined,
  startDate: `${new Date().getTime()}`,
  residualValueRate: undefined
}

const { crud, form, CRUD } = regForm(defaultForm, formRef)

const validateQuantity = (rule, value, callback) => {
  if (!value) {
    callback(new Error('填写数据必须大于0'))
  }
  callback()
}

const rules = {
  name: [{ required: true, message: '请输入设备名称', trigger: 'blur' }],
  num: [{ required: true, validator: validateQuantity, trigger: 'blur' }],
  originalValue: [{ required: true, validator: validateQuantity, trigger: 'blur' }],
  depreciationYear: [{ required: true, validator: validateQuantity, trigger: 'blur' }],
  residualValueRate: [{ required: true, validator: validateQuantity, trigger: 'blur' }],
  startDate: [{ required: true, message: '请选择折旧开始日期', trigger: 'blur' }]
}

const annualDepreciationRate = computed(() => {
  // （（1-净残值）/ 使用年限）
  return form.residualValueRate && form.depreciationYear
    ? (((100 - form.residualValueRate) / 100 / form.depreciationYear) * 100).toFixed(2)
    : ''
})

const annualDepreciationAmount = computed(() => {
  // 初始价值*（（1-净残值）/ 使用年限）
  return form.num && form.originalValue && form.residualValueRate && form.depreciationYear
    ? (form.num * form.originalValue * ((100 - form.residualValueRate) / 100 / form.depreciationYear)).toFixed(2)
    : ''
})

const monthValueDepreciationRate = computed(() => {
  // （（1-净残值）/ 使用年限）/ 12
  return form.residualValueRate && form.depreciationYear
    ? (((100 - form.residualValueRate) / 100 / form.depreciationYear / 12) * 100).toFixed(2)
    : ''
})

const monthValueDepreciationAmount = computed(() => {
  // 初始价值*（（1-净残值）/ 使用年限 / 12）
  return form.num * form.originalValue && form.residualValueRate && form.depreciationYear
    ? (form.num * form.originalValue * ((100 - form.residualValueRate) / 100 / form.depreciationYear / 12)).toFixed(2)
    : ''
})

function yearChange(val) {
  if (val) {
    const timestamp = moment(+form.startDate).add(val, 'years').valueOf() // 获取xx年后的时间戳
    const endOfDay = moment(timestamp).endOf('day') // 获取当天 24 点的时间
    form.endDate = `${endOfDay.valueOf()}`
  }
}

function endChange(val) {
  const duration = moment.duration(form.endDate - form.startDate) // 计算时间差
  form.depreciationYear = toFixed(duration.asYears(), 1) // 将时间差换算成年份
}

// 编辑之前
CRUD.HOOK.beforeToEdit = (crud, form) => {
  form.residualValueRate = form.residualValueRate * 100
}

// 提交前
CRUD.HOOK.beforeSubmit = async () => {
  const valid = await formRef.value.validate()
  if (!valid) return false
  form.residualValueRate = form.residualValueRate / 100
}
</script>

<style lang="scss" scoped>
.hint {
  color: #999;
}
</style>
