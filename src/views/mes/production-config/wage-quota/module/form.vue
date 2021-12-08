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
      <el-form-item label="名称" prop="name">
        {{ form.name }}
      </el-form-item>
      <el-form-item label="工序" prop="processId">
        <common-select
          v-model="form.processId"
          :options="form.processOption || []"
          size="small"
          type="other"
          clearable
          style="width: 250px"
          placeholder="请选择工序"
          @change="processChange"
        />
      </el-form-item>
      <template v-if="form.processId">
        <el-form-item v-for="item in wageQuotaTypeEnum.ENUM" :key="item.V" :label="`${item.L}单价`" :prop="item.F">
          <el-input
            v-model="form.processOption[form.processId].wageQuota[item.F]"
            placeholder="请输入单价"
            :precision="2"
            oninput="value=value.replace(/[^0-9.]/g,'')"
            style="width: 250px"
          >
            <template v-slot:append>{{ item.unit }}</template>
          </el-input>
          <el-checkbox v-model="checked[item.K]" style="margin-left: 10px" @change="wageQuotaTypeChange($event, item.K)" />
        </el-form-item>
      </template>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { ref } from 'vue'
import { ElMessage } from 'element-plus'

import { isNotBlank } from '@data-type/index'
import { wageQuotaTypeEnum, processMaterialListTypeEnum as typeEnum } from '@enum-ms/mes'

import { regForm } from '@compos/use-crud'

const formRef = ref()
const checked = ref({})
const originWageQuotaType = ref()

const defaultForm = {
  id: undefined,
  processId: undefined
}

const { crud, form, CRUD } = regForm(defaultForm, formRef)

const rules = {
  processId: [{ required: true, message: '请选择工序', trigger: 'change' }]
}

CRUD.HOOK.beforeToCU = () => {
  form.processId = form.process[0].processId
  processChange()
}

function processChange() {
  checked.value = {}
  const type = form.processOption[form.processId].wageQuota.wageQuotaType
  if (isNotBlank(type)) {
    originWageQuotaType.value = type
    checked.value[wageQuotaTypeEnum.VK[type]] = true
  } else {
    // 构件默认‘重量单价’ ；围护默认‘长度单位’
    if (form.sequenceType === typeEnum.ENCLOSURE.V) {
      checked.value[wageQuotaTypeEnum.LENGTH.K] = true
    } else {
      checked.value[wageQuotaTypeEnum.WEIGHT.K] = true
    }
  }
}

// 只能勾选一个单价
function wageQuotaTypeChange(status, K) {
  if (status) {
    for (const item in checked.value) {
      if (item !== K) {
        checked.value[item] = false
      }
    }
  }
}

// 提交前
CRUD.HOOK.beforeSubmit = async () => {
  const priceKey = []
  let checkedKey = ''
  for (const item in wageQuotaTypeEnum.ENUM) {
    if (isNotBlank(form.processOption[form.processId].wageQuota[wageQuotaTypeEnum[wageQuotaTypeEnum[item].K].F])) {
      priceKey.push(item)
    }
    if (checked.value[item]) {
      checkedKey = item
    }
  }
  if (priceKey.length === 0) {
    ElMessage.warning('至少填写一个单价')
    return false
  }
  if (checkedKey && !priceKey.includes(checkedKey)) {
    ElMessage.warning('请填写勾选的单价')
    return false
  }
  if (priceKey.length > 1 && !checkedKey) {
    ElMessage.warning('请勾选的一种单价作为工资结算依据')
    return false
  }
  form.processOption[form.processId].wageQuota.wageQuotaType = checkedKey
    ? wageQuotaTypeEnum[checkedKey].V
    : wageQuotaTypeEnum[priceKey[0]].V
  const { areaPice, lengthPrice, weightPrice, wageQuotaType } = form.processOption[form.processId].wageQuota
  const { productProcessId } = form.processOption[form.processId]
  form.value = Object.assign(form, { areaPice, lengthPrice, weightPrice, wageQuotaType, productProcessId })
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
::v-deep(.el-input-group__append) {
  width: 100px;
  text-align: center;
}
</style>
