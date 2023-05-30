<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="`${isEdit ? '编辑' : '新增'}${costTypeEnum.VL[crud.query.type]}`"
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
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="160px">
        <el-form-item label="起始日期" prop="startDate">
          <el-date-picker
            v-model="form.startDate"
            type="date"
            value-format="x"
            placeholder="选择起始日期"
            :disabled="true"
            style="width: 270px"
          />
        </el-form-item>
        <el-form-item label="结束日期" prop="endDate">
          <el-date-picker
            v-model="form.endDate"
            type="date"
            value-format="x"
            placeholder="选择结束日期"
            style="width: 270px"
            :disabled-date="disabledDate"
          />
        </el-form-item>
        <el-form-item :label="`用${crud.query.type === costTypeEnum.ELECTRIC_COST.V ? '电度数（kW·h）' : '水量（吨）'}`" prop="usedMete">
          <el-input-number
            v-show-thousand
            v-model="form.usedMete"
            style="width: 270px"
            :placeholder="`请输入${crud.query.type === costTypeEnum.ELECTRIC_COST.V ? '用电度数' : '用水量'}`"
            controls-position="right"
            :min="0"
            :max="9999999999"
          />
        </el-form-item>
        <el-form-item :label="`${costTypeEnum.VL[crud.query.type]}（元）`" prop="totalAmount">
          <el-input-number
            v-show-thousand
            v-model="form.totalAmount"
            style="width: 270px"
            :placeholder="`请输入${costTypeEnum.VL[crud.query.type]}`"
            controls-position="right"
            :min="0"
            :max="9999999999"
          />
        </el-form-item>
      </el-form>
    </div>
  </common-dialog>
</template>

<script setup>
import { getDate } from '@/api/contract/expense-entry/water-electricity-cost'
import { ref, computed } from 'vue'

import { costTypeEnum } from '@enum-ms/contract'

import { regForm } from '@compos/use-crud'

// 是否是编辑状态
const isEdit = computed(() => {
  return crud.status.edit > 0
})
const formRef = ref()

const defaultForm = {
  id: undefined,
  usedMete: undefined,
  totalAmount: undefined,
  startDate: undefined,
  endDate: undefined
}

const { crud, form, CRUD } = regForm(defaultForm, formRef)

const validateQuantity = (rule, value, callback) => {
  if (!value) {
    callback(new Error('填写数据必须大于0'))
  }
  callback()
}
const rules = {
  startDate: [{ required: true, message: '请选择起始日期', trigger: 'blur' }],
  endDate: [{ required: true, message: '请选择结束日期', trigger: 'blur' }],
  usedMete: [{ required: true, validator: validateQuantity, trigger: 'blur' }],
  totalAmount: [{ required: true, validator: validateQuantity, trigger: 'blur' }]
}

function disabledDate(time) {
  return time > new Date() || time < form.startDate
}

// 设置时间
async function setDate() {
  try {
    const data = await getDate({ type: crud.query.type, childType: crud.query.childType })
    form.startDate = `${data.startDate || ''}`
    form.endDate = `${data.endDate || ''}`
  } catch (error) {
    console.log('获取水电费新增的时间', error)
  }
}

// 新增需要获取开始、结束时间
CRUD.HOOK.afterToAdd = () => {
  setDate()
}

// 提交前
CRUD.HOOK.beforeSubmit = async () => {
  const valid = await formRef.value.validate()
  if (!valid) return false
  form.type = crud.query.type
  form.childType = crud.query.childType
}
</script>
