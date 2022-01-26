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
      <el-tag type="success" v-if="contractInfo.contractAmount">{{'可收款余额:'+toThousand(contractInfo.contractAmount-totalAmount)}}</el-tag>
      <el-form ref="formRef" :model="form" size="small" label-width="140px">
        <common-table
          ref="detailRef"
          border
          :data="form.list"
          :max-height="maxHeight"
          style="width: 100%;margin-top:10px;"
          class="table-form"
          :cell-class-name="wrongCellMask"
        >
          <el-table-column label="序号" type="index" align="center" width="50" />
          <el-table-column key="invoiceDate" prop="invoiceDate" label="*开票日期" align="center" width="160">
            <template v-slot="scope">
              <template v-if="scope.row.type===2">
                <span>合计</span>
              </template>
              <template v-else>
                <el-date-picker
                  v-if="scope.row.isModify"
                  v-model="scope.row.invoiceDate"
                  type="date"
                  size="small"
                  value-format="x"
                  placeholder="选择日期"
                  style="width:100%"
                  :disabledDate="(date) => {if (scope.row.invoiceDate) { return date.getTime() > scope.row.invoiceDate } else { return date.getTime() < new Date().getTime() - 1 * 24 * 60 * 60 * 1000 }}"
                />
                <template v-else>
                  <div>{{ scope.row.invoiceDate? parseTime(scope.row.invoiceDate,'{y}-{m}-{d}'): '-' }}</div>
                </template>
              </template>
            </template>
          </el-table-column>
          <el-table-column key="invoiceAmount" prop="invoiceAmount" label="*开票额" align="center" min-width="170" class="money-column">
            <el-table-column key="invoiceAmount" prop="invoiceAmount" label="金额" align="center" min-width="85">
              <template v-slot="scope">
              <template v-if="scope.row.type===2">
                <span>{{totalAmount?toThousand(totalAmount): totalAmount}}</span>
              </template>
              <template v-else>
                <el-input-number
                  v-if="scope.row.isModify"
                  v-show-thousand
                  v-model.number="scope.row.invoiceAmount"
                  :min="0"
                  :max="contractInfo.contractAmount-totalAmount"
                  :step="100"
                  :precision="DP.YUAN"
                  placeholder="开票额(元)"
                  controls-position="right"
                  @change="moneyChange(scope.row)"
                />
                <div v-else>{{ scope.row.invoiceAmount && scope.row.invoiceAmount>0? toThousand(scope.row.invoiceAmount): scope.row.invoiceAmount }}</div>
              </template>
            </template>
            </el-table-column>
            <el-table-column key="invoiceAmount1" prop="invoiceAmount" label="大写" align="center" min-width="85" :show-overflow-tooltip="true">
              <template v-slot="scope">
              <template v-if="scope.row.type===2">
                <span>{{totalAmount?'('+digitUppercase(totalAmount)+')':''}}</span>
              </template>
              <template v-else>
                <div>{{scope.row.invoiceAmount?'('+digitUppercase(scope.row.invoiceAmount)+')':''}}</div>
              </template>
            </template>
            </el-table-column>
          </el-table-column>
          <el-table-column key="invoiceType" prop="invoiceType" label="*发票类型" align="center" width="120">
            <template v-slot="scope">
              <common-select
                v-if="scope.row.isModify"
                v-model="scope.row.invoiceType"
                :options="invoiceTypeEnum.ENUM"
                type="enum"
                size="small"
                clearable
                class="filter-item"
                placeholder="发票类型"
                style="width: 100%"
                @change="invoiceTypeChange(scope.row)"
              />
              <div v-else>{{ scope.row.invoiceType? invoiceTypeEnum.VL[scope.row.invoiceType]: '' }}</div>
            </template>
          </el-table-column>
          <el-table-column key="taxRate" prop="taxRate" label="税率" align="center" width="110">
            <template v-slot="scope">
              <div v-if="scope.row.invoiceType !== invoiceTypeEnum.RECEIPT.V && scope.row.isModify">
                <el-input-number
                  v-model="scope.row.taxRate"
                  :step="1"
                  :min="0"
                  :max="100"
                  :precision="0"
                  :controls="false"
                  controls-position="right"
                  class="input-underline"
                  style="width: 70px; text-align: center"
                  placeholder="0-100"
                  @change="taxMoney(scope.row)"
                />%
              </div>
              <div v-else>{{ scope.row.taxRate? scope.row.taxRate+'%': '' }}</div>
            </template>
          </el-table-column>
          <el-table-column key="invoiceUnit" prop="invoiceUnit" label="*开票单位" align="center" min-width="120" :show-overflow-tooltip="true">
            <template v-slot="scope">
              <div>{{ scope.row.invoiceUnit }}</div>
            </template>
          </el-table-column>
          <el-table-column key="collectionUnit" prop="collectionUnit" label="*收票单位" align="center" min-width="120" :show-overflow-tooltip="true">
            <template v-slot="scope">
              <el-input
                v-if="scope.row.isModify"
                v-model="scope.row.collectionUnit"
                placeholder="收票单位"
                style="width:100%;"
                maxlength="50"
              />
              <div v-else>{{ scope.row.collectionUnit  }}</div>
            </template>
          </el-table-column>
          <el-table-column prop="invoiceNo" label="*发票号码" align="center" min-width="150">
            <template v-slot="scope">
              <el-input v-if="scope.row.isModify" v-model="scope.row.invoiceNo" type="text" placeholder="发票号码" style="width: 120px" @change="checkInvoiceNo(scope.row,scope.$index)" maxlength="8"/>
              <span v-else>{{ scope.row.invoiceNo  }}</span>
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
import { ref, inject, nextTick, defineProps, watch } from 'vue'
import { regForm } from '@compos/use-crud'
import { ElMessage } from 'element-plus'
import { DP } from '@/settings/config'
import { validate } from '@compos/form/use-table-validate'
import useMaxHeight from '@compos/use-max-height'
import { digitUppercase } from '@/utils/data-type/number'
import { toThousand } from '@data-type/number'
import { invoiceTypeEnum } from '@enum-ms/finance'

const formRef = ref()
const detailRef = ref()
const defaultForm = {
  projectId: undefined,
  list: []
}

const { CRUD, crud, form } = regForm(defaultForm, formRef)
const projectId = inject('projectId')
const contractInfo = inject('contractInfo')
const totalAmount = inject('totalAmount')
const extraAmount = ref(0)
const invoiceNoArr = ref([])

const props = defineProps({
  existInvoiceNo: {
    type: Array,
    default: () => {}
  }
})

watch(
  () => props.existInvoiceNo,
  (val) => {
    invoiceNoArr.value = []
    if (val) {
      if (props.existInvoiceNo.length > 0) {
        invoiceNoArr.value = Object.assign([], props.existInvoiceNo)
      }
    }
  },
  { deep: true, immediate: true }
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.invoiceAddForm',
  paginate: true,
  extraHeight: 40
})

const tableRules = {
  invoiceDate: [{ required: true, message: '请选择开票日期', trigger: 'change' }],
  invoiceAmount: [{ required: true, message: '请选择开票额', trigger: 'change', type: 'number' }],
  taxRate: [{ required: true, message: '请输入税率', trigger: 'blur' }],
  invoiceType: [{ required: true, message: '请选择发票类型', trigger: 'change' }],
  invoiceNo: [{ required: true, message: '请输入发票号', trigger: 'blur' }],
  collectionUnit: [{ required: true, message: '请输入收票单位', trigger: 'blur' }]
}

const otherRules = {
  invoiceDate: [{ required: true, message: '请选择开票日期', trigger: 'change' }],
  invoiceAmount: [{ required: true, message: '请选择开票额', trigger: 'change', type: 'number' }],
  invoiceType: [{ required: true, message: '请选择发票类型', trigger: 'change' }],
  invoiceNo: [{ required: true, message: '请输入发票号', trigger: 'blur' }],
  collectionUnit: [{ required: true, message: '请输入收票单位', trigger: 'blur' }]
}

function wrongCellMask({ row, column }) {
  if (!row) return
  let rules = {}
  if (row.invoiceType !== invoiceTypeEnum.RECEIPT.V) {
    rules = tableRules
  } else {
    rules = otherRules
  }
  let flag = true
  if (row.verify && Object.keys(row.verify) && Object.keys(row.verify).length > 0) {
    if (row.verify[column.property] === false) {
      flag = validate(column.property, rules[column.property], row[column.property], row)
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
    invoiceAmount: undefined,
    invoiceDate: undefined,
    invoiceType: undefined,
    invoiceNo: undefined,
    taxRate: undefined,
    tax: undefined,
    invoiceUnit: contractInfo.value.companyBankAccountList && contractInfo.value.companyBankAccountList.length > 0 ? contractInfo.value.companyBankAccountList[0].companyName : undefined,
    invoiceUnitId: contractInfo.value.companyBankAccountList && contractInfo.value.companyBankAccountList.length > 0 ? contractInfo.value.companyBankAccountList[0].companyId : undefined,
    collectionUnit: contractInfo.value.customerUnit || undefined,
    projectId: projectId,
    isModify: true
  })
}

// function moneyChange(row) {
//   extraAmount.value = 0
//   form.list.map(v => {
//     if (v.collectionAmount) {
//       extraAmount.value += v.collectionAmount
//     }
//   })
//   if (extraAmount.value > (contractInfo.value.contractAmount - totalAmount.value)) {
//     const num = row.collectionAmount - (extraAmount.value - (contractInfo.value.contractAmount - totalAmount.value))
//     nextTick(() => {
//       row.collectionAmount = num || 0
//       extraAmount.value = 0
//       form.list.map(v => {
//         if (v.collectionAmount) {
//           extraAmount.value += v.collectionAmount
//         }
//       })
//     })
//   }
// }

function invoiceTypeChange(row) {
  row.taxRate = undefined
}

function moneyChange(row) {
  extraAmount.value = 0
  form.list.map(v => {
    if (v.invoiceAmount) {
      extraAmount.value += v.invoiceAmount
    }
  })
  if (extraAmount.value > (contractInfo.value.contractAmount - totalAmount.value)) {
    const num = row.invoiceAmount - (extraAmount.value - (contractInfo.value.contractAmount - totalAmount.value))
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
  } else {
    taxMoney(row)
  }
}

function taxMoney(row) {
  if (row.invoiceAmount && row.taxRate) {
    row.tax = row.invoiceAmount * row.taxRate / 100
  }
}
function checkInvoiceNo(row) {
  if (row.invoiceNo) {
    const val = invoiceNoArr.value.find(v => v.dataIndex === row.dataIndex)
    if (invoiceNoArr.value.findIndex(v => v.invoiceNo === row.invoiceNo) > -1) {
      ElMessage({ message: '发票号已存在，请重新填写', type: 'error' })
      row.invoiceNo = undefined
      if (val) {
        val.invoiceNo = undefined
      }
    } else {
      if (val) {
        val.invoiceNo = row.invoiceNo
      } else {
        invoiceNoArr.value.push({
          invoiceNo: row.invoiceNo,
          dataIndex: row.dataIndex
        })
      }
    }
  }
}
CRUD.HOOK.beforeValidateCU = (crud, form) => {
  if (crud.form.list.length <= 0) {
    ElMessage({ message: '请添加开票明细', type: 'error' })
    return false
  }
  const rules = tableRules
  let flag = true
  let moneyFlag = true
  crud.form.list.map(row => {
    row.verify = {}
    for (const rule in rules) {
      row.verify[rule] = validate(rule, rules[rule], row[rule], row)
      if (!row.verify[rule]) {
        flag = false
      }
    }
    if (row.collectionAmount === 0) {
      moneyFlag = false
    }
  })
  if (!flag) {
    ElMessage.error('请填写表格中标红数据')
    return false
  }
  if (!moneyFlag) {
    ElMessage.error('开票金额必须大于0')
    return false
  }
  crud.form.projectId = projectId
}
</script>
<style lang="scss" scoped>
.add-row-box {
  text-align: center;
  margin-top: 20px;
}
</style>
