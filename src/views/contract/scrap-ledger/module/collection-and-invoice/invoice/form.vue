<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    :wrapper-closable="false"
    size="100%"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <template #content>
      <!-- <el-tag type="success" v-if="contractInfo.contractAmount">{{'合同金额:'+toThousand(contractInfo.contractAmount,decimalPrecision.contract)}}</el-tag>
      <el-tag type="success" size="medium" v-if="currentRow.settlementAmount" style="margin-left:5px;">{{'结算金额:'+toThousand(currentRow.settlementAmount,decimalPrecision.contract)}}</el-tag> -->
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
          <el-table-column key="invoiceDate" prop="invoiceDate" label="*开票日期" align="center" width="160">
            <template v-slot="scope">
              <el-date-picker
                v-if="scope.row.isModify"
                v-model="scope.row.invoiceDate"
                type="date"
                size="small"
                value-format="x"
                placeholder="选择日期"
                style="width:100%"
              />
              <template v-else>
                <div>{{ scope.row.invoiceDate? parseTime(scope.row.invoiceDate,'{y}-{m}-{d}'): '-' }}</div>
              </template>
            </template>
          </el-table-column>
          <el-table-column key="invoiceAmount2" prop="invoiceAmount2" label="*开票额" align="center" min-width="180" class="money-column">
            <el-table-column key="invoiceAmount" prop="invoiceAmount" label="金额" align="center" min-width="85">
              <template v-slot="scope">
                <el-input-number
                    v-if="scope.row.isModify"
                    v-show-thousand
                    v-model.number="scope.row.invoiceAmount"
                    :min="-9999999999"
                    :max="currentRow.settlementAmount?currentRow.settlementAmount-totalAmount:9999999999"
                    :step="100"
                    :precision="decimalPrecision.contract"
                    placeholder="开票额(元)"
                    controls-position="right"
                    @change="moneyChange(scope.row)"
                  />
                  <div v-else>{{ scope.row.invoiceAmount && scope.row.invoiceAmount>0? toThousand(scope.row.invoiceAmount,decimalPrecision.contract): scope.row.invoiceAmount }}</div>
              </template>
            </el-table-column>
            <el-table-column key="invoiceAmount1" prop="invoiceAmount1" label="大写" align="center" min-width="85" :show-overflow-tooltip="true">
              <template v-slot="scope">
                <div>{{scope.row.invoiceAmount?digitUppercase(scope.row.invoiceAmount):''}}</div>
              </template>
            </el-table-column>
          </el-table-column>
          <el-table-column key="invoiceType" prop="invoiceType" label="*发票类型" align="center" width="120">
            <template v-slot="scope">
              <!-- <div>{{ scope.row.invoiceType? invoiceTypeEnum.VL[scope.row.invoiceType]: '-' }}</div> -->
              <common-select
                v-model="scope.row.invoiceType"
                :options="invoiceTypeEnum.ENUM"
                type="enum"
                size="small"
                clearable
                placeholder="发票类型"
                style="width:100%"
              />
            </template>
          </el-table-column>
          <el-table-column key="taxRate" prop="taxRate" label="税率" align="center" width="80">
            <template v-slot="scope">
              <div v-if="scope.row.invoiceType !== invoiceTypeEnum.RECEIPT.V && currentRow.isTax !== isTaxContractEnum.NO.V && scope.row.isModify">
                <el-input-number
                  v-model="scope.row.taxRate"
                  :step="1"
                  :min="0"
                  :max="100"
                  :precision="0"
                  :controls="false"
                  controls-position="right"
                  class="input-underline"
                  style="width: 60px; text-align: center"
                  placeholder="0-100"
                  @change="taxMoney(scope.row)"
                />%
              </div>
              <div v-else>{{ scope.row.taxRate? scope.row.taxRate+'%': '-' }}</div>
            </template>
          </el-table-column>
          <el-table-column key="noTaxAmount" prop="noTaxAmount" label="不含税金额" align="center" width="70">
            <template v-slot="scope">
              <span>{{scope.row.noTaxAmount && scope.row.noTaxAmount>0? toThousand(scope.row.noTaxAmount,decimalPrecision.contract): scope.row.noTaxAmount}}</span>
            </template>
          </el-table-column>
          <el-table-column key="invoiceUnit" prop="invoiceUnit" label="*开票单位" align="center" min-width="120" :show-overflow-tooltip="true">
            <template v-slot="scope">
              <div>{{ scope.row.invoiceUnit }}</div>
            </template>
          </el-table-column>
          <el-table-column key="collectionUnit" prop="collectionUnit" label="*收票单位" align="center" min-width="120" :show-overflow-tooltip="true">
            <template v-slot="scope">
              <!-- <el-input
                v-if="scope.row.isModify"
                v-model.trim="scope.row.collectionUnit"
                placeholder="收票单位"
                style="width:100%;"
                maxlength="50"
              /> -->
              <div>{{ scope.row.collectionUnit  }}</div>
            </template>
          </el-table-column>
          <el-table-column prop="invoiceNo" label="*发票号码" align="center" min-width="130">
            <template v-slot="scope">
              <el-input v-if="scope.row.isModify" v-model.trim="scope.row.invoiceNo" type="text" placeholder="发票号码" style="width: 100%;" />
              <span v-else>{{ scope.row.invoiceNo  }}</span>
            </template>
          </el-table-column>
          <el-table-column prop="invoiceContent" label="发票内容" align="center" min-width="130">
            <template v-slot="scope">
              <el-input v-if="scope.row.isModify" v-model.trim="scope.row.invoiceContent" type="text" placeholder="发票内容" style="width: 100%;"/>
              <span v-else>{{ scope.row.invoiceContent  }}</span>
            </template>
          </el-table-column>
          <el-table-column prop="remark" label="备注" align="center" min-width="130">
            <template v-slot="scope">
              <el-input v-if="scope.row.isModify" v-model.trim="scope.row.remark" type="textarea" placeholder="备注" style="width: 100%;" maxlength="200" />
              <span v-else>{{ scope.row.remark  }}</span>
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
import { ElMessage } from 'element-plus'

import { regForm } from '@compos/use-crud'
import { isNotBlank } from '@data-type/index'
import useMaxHeight from '@compos/use-max-height'
import { digitUppercase, toThousand } from '@/utils/data-type/number'
import { isTaxContractEnum } from '@enum-ms/contract'
import { invoiceTypeEnum } from '@enum-ms/finance'
import useTableValidate from '@compos/form/use-table-validate'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const formRef = ref()
const detailRef = ref()
const defaultForm = {
  projectId: undefined,
  list: []
}

const { decimalPrecision } = useDecimalPrecision()

const { CRUD, crud, form } = regForm(defaultForm, formRef)
const contractInfo = inject('contractInfo')
const totalAmount = inject('totalAmount')
// const invoiceNoArr = ref([])
const extraAmount = ref(0)

const props = defineProps({
  existInvoiceNo: {
    type: Array,
    default: () => {}
  },
  projectId: {
    type: [String, Number],
    default: undefined
  },
  currentRow: {
    type: Object,
    default: () => {}
  }
})

// watch(
//   () => props.existInvoiceNo,
//   (val) => {
//     invoiceNoArr.value = []
//     if (val) {
//       if (props.existInvoiceNo.length > 0) {
//         invoiceNoArr.value = Object.assign([], props.existInvoiceNo)
//       }
//     }
//   },
//   { deep: true, immediate: true }
// )

const { maxHeight } = useMaxHeight({
  wrapperBox: '.invoiceAddForm',
  paginate: true,
  extraHeight: 40
})

const validateTaxRate = (value, row) => {
  if (row.invoiceType !== invoiceTypeEnum.RECEIPT.V && props.currentRow.isTax !== isTaxContractEnum.NO.V) return !!value
  return true
}

const validateInvoiceType = (value, row) => {
  if (props.currentRow.isTax !== isTaxContractEnum.NO.V) return !!value
  return true
}

// 金额校验
const validateAmount = (value, row) => {
  if (!isNotBlank(value)) return false
  return true
}

const tableRules = {
  invoiceDate: [{ required: true, message: '请选择开票日期', trigger: 'change' }],
  invoiceAmount: [{ validator: validateAmount, message: '请选择开票额', trigger: 'change', type: 'number' }],
  taxRate: [{ validator: validateTaxRate, message: '请输入税率', trigger: 'blur' }],
  invoiceType: [{ validator: validateInvoiceType, message: '请选择发票类型', trigger: 'change' }],
  invoiceNo: [{ required: true, message: '请输入发票号', trigger: 'blur' }],
  collectionUnit: [{ required: true, message: '请输入收票单位', trigger: 'blur' }]
}

const { tableValidate, wrongCellMask } = useTableValidate({ rules: tableRules }) // 表格校验

function deleteRow(index) {
  form.list.splice(index, 1)
}

function addRow() {
  form.list.push({
    invoiceAmount: undefined,
    invoiceDate: undefined,
    // invoiceType: props.currentRow.invoiceType,
    invoiceType: undefined,
    invoiceNo: undefined,
    taxRate: props.currentRow.taxRate ? props.currentRow.taxRate * 100 : undefined,
    tax: undefined,
    invoiceUnit: contractInfo.value.branchCompanyName || undefined,
    invoiceUnitId: contractInfo.value.branchCompanyId || undefined,
    collectionUnit: contractInfo.value.purchaser || undefined,
    contractWasteId: contractInfo.value.contractWasteId,
    dataIndex: form.list.length + 1,
    isModify: true
  })
}

function moneyChange(row) {
  if (props.currentRow.settlementAmount) {
    extraAmount.value = 0
    form.list.map(v => {
      if (v.invoiceAmount) {
        extraAmount.value += v.invoiceAmount
      }
    })
    if (extraAmount.value > (props.currentRow.settlementAmount - totalAmount.value)) {
      const num = row.invoiceAmount - (extraAmount.value - (props.currentRow.settlementAmount - totalAmount.value))
      // 解决修改失效
      nextTick(() => {
        row.invoiceAmount = num || 0
        taxMoney(row)
        extraAmount.value = 0
        form.list.map(v => {
          if (v.invoiceAmount) {
            extraAmount.value += v.invoiceAmount
          }
        })
      })
    }
  }
  taxMoney(row)
}

function taxMoney(row) {
  if (isNotBlank(row.invoiceAmount) && row.taxRate) {
    row.tax = row.invoiceAmount * row.taxRate / 100
    row.noTaxAmount = (row.invoiceAmount / (1 + row.taxRate / 100)).toFixed(decimalPrecision.value.contract)
  } else {
    if (row.invoiceType === invoiceTypeEnum.RECEIPT.V) {
      row.noTaxAmount = row.invoiceAmount
    }
  }
}
// function checkInvoiceNo(row) {
//   const val = invoiceNoArr.value.find(v => v.dataIndex === row.dataIndex)
//   if (val) {
//     if (row.invoiceNo) {
//       if (val.invoiceNo === row.invoiceNo) {
//         return
//       }
//       if (invoiceNoArr.value.findIndex(v => v.invoiceNo === row.invoiceNo) > -1) {
//         ElMessage({ message: '发票号已存在，请重新填写', type: 'error' })
//         row.invoiceNo = undefined
//       } else {
//         val.invoiceNo = row.invoiceNo
//       }
//     } else {
//       val.invoiceNo = undefined
//     }
//   } else {
//     if (invoiceNoArr.value.findIndex(v => v.invoiceNo === row.invoiceNo) > -1) {
//       ElMessage({ message: '发票号已存在，请重新填写', type: 'error' })
//       row.invoiceNo = undefined
//     } else {
//       invoiceNoArr.value.push({
//         invoiceNo: row.invoiceNo,
//         dataIndex: row.dataIndex
//       })
//     }
//   }
// }
CRUD.HOOK.beforeValidateCU = (crud, form) => {
  if (crud.form.list.length <= 0) {
    ElMessage({ message: '请添加开票明细', type: 'error' })
    return false
  }
  const { validResult, dealList } = tableValidate(crud.form.list)
  if (validResult) {
    crud.form.list = dealList
  } else {
    return validResult
  }
  crud.form.projectId = props.projectId
}
</script>
<style lang="scss" scoped>
.add-row-box {
  text-align: center;
  margin-top: 20px;
}
.table-form {
  ::v-deep(.el-table__cell .cell) {
  padding: 0 2px;
}
}

</style>
