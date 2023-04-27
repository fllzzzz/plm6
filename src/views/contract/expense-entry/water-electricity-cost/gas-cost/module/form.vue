<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="`${isEdit ? '编辑' : '新增'}气体`"
    :show-close="false"
    width="450px"
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
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="100px">
        <el-form-item label="气体分类" prop="wmsClassificationId">
          <el-cascader
            v-model="form.wmsClassificationId"
            :options="props.gasTree"
            :props="{ value: 'id', label: 'name', children: 'children', expandTrigger: 'hover', emitPath: false }"
            show-all-levels
            clearable
            placeholder="请选择气体分类"
            style="width: 270px"
            @change="handleGasChange"
          />
        </el-form-item>
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
        <el-form-item label="气体单位" prop="accountingUnit">
          <span>{{ form.accountingUnit }}</span>
        </el-form-item>
        <el-form-item label="气体用量" prop="usedMete">
          <el-input-number v-model="form.usedMete" style="width: 270px" placeholder="请输入用量" controls-position="right" :min="0" />
        </el-form-item>
        <el-form-item label="总额（元）" prop="totalAmount">
          <el-input-number
            v-show-thousand
            v-model="form.totalAmount"
            style="width: 270px"
            placeholder="请输入总额"
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
import { ref, defineProps, computed } from 'vue'
import { getDate } from '@/api/contract/expense-entry/gas-cost'

import { regForm } from '@compos/use-crud'

const props = defineProps({
  rowDetail: {
    type: Object,
    default: () => {}
  },
  gasTree: {
    type: Array,
    default: () => []
  },
  lastGasKV: {
    type: Object,
    default: () => {}
  }
})

const formRef = ref()

const defaultForm = {
  id: undefined,
  wmsClassificationId: undefined,
  accountingUnit: undefined,
  usedMete: undefined,
  totalAmount: undefined,
  startDate: undefined,
  endDate: undefined
}

const { crud, form, CRUD } = regForm(defaultForm, formRef)

// 是否是编辑状态
const isEdit = computed(() => {
  return crud.status.edit > 0
})

const validateQuantity = (rule, value, callback) => {
  if (!value) {
    callback(new Error('填写数据必须大于0'))
  }
  callback()
}

const validateAccountingUnit = (rule, value, callback) => {
  if (!form.wmsClassificationId) {
    callback(new Error('请先选择气体分类'))
  } else if (!form.accountingUnit) {
    callback(new Error('请联系管理员进行对科目进行计量配置'))
  }
  callback()
}

const rules = {
  startDate: [{ required: true, message: '请选择起始日期', trigger: 'blur' }],
  endDate: [{ required: true, message: '请选择结束日期', trigger: 'blur' }],
  wmsClassificationId: [{ required: true, message: '请选择气体分类', trigger: 'blur' }],
  accountingUnit: [{ required: true, validator: validateAccountingUnit, trigger: 'blur' }],
  usedMete: [{ required: true, validator: validateQuantity, trigger: 'blur' }],
  totalAmount: [{ required: true, validator: validateQuantity, trigger: 'blur' }]
}

// 选择气体
function handleGasChange(id) {
  console.log('id: ', id)
  form.accountingUnit = props.lastGasKV[id]?.accountingUnit
}

// 禁止时间
function disabledDate(time) {
  return time > new Date() || time < form.startDate
}

// 设置时间
async function setDate() {
  try {
    const data = await getDate()
    form.startDate = `${data.startDate || ''}`
    form.endDate = `${data.endDate || ''}`
  } catch (error) {
    console.log('获取气体新增的时间', error)
  }
}

// 新增需要获取开始、结束时间
CRUD.HOOK.afterToAdd = (crud, form) => {
  setDate()
  // 末级
  if (props.rowDetail?.id && !props.rowDetail.children) {
    form.wmsClassificationId = props.rowDetail.id
    form.accountingUnit = props.rowDetail.accountingUnit
  }
}

// 提交前
CRUD.HOOK.beforeSubmit = async () => {
  const valid = await formRef.value.validate()
  if (!valid) return false
}
</script>
