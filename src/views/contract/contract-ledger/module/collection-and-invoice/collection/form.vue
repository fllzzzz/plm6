<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    :wrapper-closable="false"
    size="90%"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <template #content>
      <el-tag type="success" v-if="contractInfo.contractAmount">{{'合同金额:'+toThousand(contractInfo.contractAmount)}}</el-tag>
      <el-tag type="success" size="medium" v-if="currentRow.settlementAmount" style="margin-left:5px;">{{'结算金额:'+toThousand(currentRow.settlementAmount)}}</el-tag>
      <el-form ref="formRef" :model="form" size="small" label-width="140px">
        <common-table
          ref="detailRef"
          border
          :data="form.list"
          :max-height="maxHeight"
          style="width: 100%;margin-top:10px;"
          class="table-form"
          :cell-class-name="wrongCellMask"
          return-source-data
          :showEmptySymbol="false"
        >
          <el-table-column label="序号" type="index" align="center" width="50" />
          <el-table-column key="collectionDate" prop="collectionDate" label="*收款日期" align="center" width="160">
            <template v-slot="scope">
              <el-date-picker
                v-if="scope.row.isModify"
                v-model="scope.row.collectionDate"
                type="date"
                size="small"
                value-format="x"
                placeholder="选择日期"
                style="width:100%"
              />
              <template v-else>
                <div>{{ scope.row.collectionDate? parseTime(scope.row.collectionDate,'{y}-{m}-{d}'): '-' }}</div>
              </template>
            </template>
          </el-table-column>
          <el-table-column key="collectionAmount2" prop="collectionAmount2" label="*收款金额" align="center" min-width="170" class="money-column">
            <el-table-column key="collectionAmount" prop="collectionAmount" label="金额" align="center" min-width="85">
              <template v-slot="scope">
                <el-input-number
                  v-if="scope.row.isModify"
                  v-show-thousand
                  v-model.number="scope.row.collectionAmount"
                  :min="-9999999999"
                  :max="currentRow.settlementAmount?currentRow.settlementAmount-totalAmount:9999999999"
                  :step="100"
                  :precision="DP.YUAN"
                  placeholder="收款金额(元)"
                  controls-position="right"
                  :key="scope.row.dataIndex?scope.row.dataIndex:scope.row.id"
                  @change="moneyChange(scope.row)"
                />
                <div v-else>{{ isNotBlank(scope.row.collectionAmount) ? toThousand(scope.row.collectionAmount): '-' }}</div>
              </template>
            </el-table-column>
            <el-table-column key="collectionAmount1" prop="collectionAmount1" label="大写" align="center" min-width="85" :show-overflow-tooltip="true">
              <template v-slot="scope">
                <div>{{scope.row.collectionAmount?digitUppercase(scope.row.collectionAmount):''}}</div>
              </template>
            </el-table-column>
          </el-table-column>
          <el-table-column key="collectionReason" prop="collectionReason" label="*收款事由" align="center" width="120">
            <template v-slot="scope">
              <common-select
                v-if="scope.row.isModify"
                v-model="scope.row.collectionReason"
                :options="dict.payment_reason"
                type="dict"
                size="small"
                clearable
                placeholder="收款事由"
                style="width:100%"
              />
              <div v-else>{{ scope.row.collectionReason && dict && dict.label && dict.label['payment_reason']? dict.label['payment_reason'][ scope.row.collectionReason]: '' }}</div>
            </template>
          </el-table-column>
          <el-table-column key="collectionMode" prop="collectionMode" label="*收款方式" align="center" width="110">
            <template v-slot="scope">
              <common-select
                v-if="scope.row.isModify"
                v-model="scope.row.collectionMode"
                :options="paymentFineModeEnum.ENUM"
                type="enum"
                size="small"
                placeholder="收款方式"
                style="width:100%;"
              />
              <div v-else>{{ scope.row.collectionMode? paymentFineModeEnum.VL[scope.row.collectionMode]: '' }}</div>
            </template>
          </el-table-column>
          <el-table-column key="collectionUnit" prop="collectionUnit" label="*收款单位" align="center" min-width="120" :show-overflow-tooltip="true">
            <template v-slot="scope">
              <div>{{ scope.row.collectionUnit }}</div>
            </template>
          </el-table-column>
          <el-table-column key="collectionBankAccountId" prop="collectionBankAccountId" :show-overflow-tooltip="true" label="*收款银行" align="center" min-width="120">
            <template v-slot="scope">
              <common-select
                v-if="scope.row.isModify"
                v-model="scope.row.collectionBankAccountId"
                :options="bankList"
                type="other"
                :dataStructure="typeProp"
                size="small"
                placeholder="收款银行"
                style="width:100%;"
                @change="bankChange(scope.row)"
              />
              <div v-else>{{ scope.row.collectionDepositBank }}</div>
            </template>
          </el-table-column>
          <el-table-column key="paymentUnit" prop="paymentUnit" label="*付款单位" align="center" min-width="120" :show-overflow-tooltip="true">
            <template v-slot="scope">
              <el-input
                v-if="scope.row.isModify"
                v-model.trim="scope.row.paymentUnit"
                placeholder="付款单位"
                style="width:100%;"
                maxlength="50"
              />
              <div v-else>{{ scope.row.paymentUnit  }}</div>
            </template>
          </el-table-column>
          <el-table-column label="操作" align="center">
            <template v-slot="scope">
              <common-button size="small" class="el-icon-delete" type="danger" @click="deleteRow(scope.$index)" />
            </template>
          </el-table-column>
        </common-table>
        <div class="add-row-box">
          <common-button
            size="mini"
            icon="el-icon-circle-plus-outline"
            type="warning"
            style="margin-right: 15px"
            @click="addRow()"
            >继续添加</common-button>
        </div>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, inject, defineProps, nextTick } from 'vue'

import { regForm } from '@compos/use-crud'
import { ElMessage } from 'element-plus'
import { DP } from '@/settings/config'
import { validate } from '@compos/form/use-table-validate'
import useMaxHeight from '@compos/use-max-height'
import useDict from '@compos/store/use-dict'
import { paymentFineModeEnum } from '@enum-ms/finance'
import { digitUppercase, toThousand } from '@/utils/data-type/number'
import { isNotBlank } from '@data-type/index'

const formRef = ref()
const detailRef = ref()
const defaultForm = {
  projectId: undefined,
  list: []
}
const props = defineProps({
  currentRow: {
    type: Object,
    default: () => {}
  },
  existInvoiceNo: {
    type: Array,
    default: () => {}
  },
  projectId: {
    type: [String, Number],
    default: undefined
  }
})
const { CRUD, crud, form } = regForm(defaultForm, formRef)
const contractInfo = inject('contractInfo')
const bankList = inject('bankList')
const totalAmount = inject('totalAmount')
const extraAmount = ref(0)
const dict = useDict(['payment_reason'])
const typeProp = { key: 'id', label: 'depositBank', value: 'id' }
const { maxHeight } = useMaxHeight({
  wrapperBox: '.collectionAddForm',
  paginate: true,
  extraHeight: 40
})

// 金额校验
const validateAmount = (value, row) => {
  if (!isNotBlank(value)) return false
  return true
}

const tableRules = {
  collectionDate: [{ required: true, message: '请选择收款日期', trigger: 'change' }],
  collectionAmount: [{ validator: validateAmount, message: '请选择收款金额', trigger: 'change', type: 'number' }],
  collectionBankAccountId: [{ required: true, message: '请选择收款银行', trigger: 'change' }],
  collectionMode: [{ required: true, message: '请选择收款方式', trigger: 'change' }],
  collectionReason: [{ required: true, message: '请选择收款事由', trigger: 'change' }],
  paymentUnit: [{ required: true, message: '请输入付款单位', trigger: 'blur' }]
}

function wrongCellMask({ row, column }) {
  if (!row) return
  const rules = tableRules
  let flag = true
  if (row.verify && Object.keys(row.verify) && Object.keys(row.verify).length > 0) {
    if (row.verify[column.property] === false) {
      flag = validate(column.property, rules[column.property], row)
    }
    if (flag) {
      row.verify[column.property] = true
    }
  }
  return flag ? '' : 'mask-td'
}

function deleteRow(index) {
  form.list.splice(index, 1)
}

function addRow() {
  form.list.push({
    collectionAmount: undefined,
    collectionBankAccount: undefined,
    collectionBankAccountId: undefined,
    collectionDate: undefined,
    collectionDepositBank: undefined,
    collectionMode: undefined,
    collectionReason: undefined,
    collectionUnit: contractInfo.value.companyBankAccountList && contractInfo.value.companyBankAccountList.length > 0 ? contractInfo.value.companyBankAccountList[0].companyName : undefined,
    collectionUnitId: contractInfo.value.companyBankAccountList && contractInfo.value.companyBankAccountList.length > 0 ? contractInfo.value.companyBankAccountList[0].companyId : undefined,
    paymentBankAccount: contractInfo.value.customerBankCode || undefined,
    paymentDepositBank: contractInfo.value.customerBankName || undefined,
    paymentUnit: contractInfo.value.customerUnit || undefined,
    projectId: props.projectId,
    isModify: true
  })
}

function moneyChange(row) {
  extraAmount.value = 0
  if (props.currentRow.settlementAmount) {
    form.list.map(v => {
      if (v.collectionAmount) {
        extraAmount.value += v.collectionAmount
      }
    })
    if (extraAmount.value > (props.currentRow.settlementAmount - totalAmount.value)) {
      const num = row.collectionAmount - (extraAmount.value - (props.currentRow.settlementAmount - totalAmount.value))
      nextTick(() => {
        row.collectionAmount = num || 0
        extraAmount.value = 0
        form.list.map(v => {
          if (v.collectionAmount) {
            extraAmount.value += v.collectionAmount
          }
        })
      })
    }
  }
}

function bankChange(row) {
  if (row.collectionBankAccountId) {
    const choseVal = bankList.value.find(v => v.id === row.collectionBankAccountId)
    row.collectionDepositBank = choseVal.depositBank
    row.collectionBankAccount = choseVal.account
  } else {
    row.collectionDepositBank = undefined
    row.collectionBankAccount = undefined
  }
}

CRUD.HOOK.beforeValidateCU = (crud, form) => {
  if (crud.form.list.length <= 0) {
    ElMessage({ message: '请添加收款明细', type: 'error' })
    return false
  }
  const rules = tableRules
  let flag = true
  crud.form.list.map(row => {
    row.verify = {}
    for (const rule in rules) {
      row.verify[rule] = validate(rule, rules[rule], row)
      if (!row.verify[rule]) {
        flag = false
      }
    }
  })
  if (!flag) {
    ElMessage.error('请填写表格中标红数据')
    return false
  }
  crud.form.projectId = props.projectId
}
</script>
<style lang="scss" scoped>
.add-row-box {
  text-align: center;
  margin-top: 20px;
}
</style>
