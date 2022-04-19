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
      <el-tag type="success" v-if="currentRow.freight">{{'运输额:'+toThousand(currentRow.freight)}}</el-tag>
      <el-form ref="formRef" :model="form" size="small" label-width="140px">
        <common-table
          ref="detailRef"
          border
          :data="form.list"
          :max-height="maxHeight"
          style="width: 100%;margin-top:10px;"
          class="table-form"
          return-source-data
          :showEmptySymbol="false"
          :cell-class-name="wrongCellMask"
        >
          <el-table-column label="序号" type="index" align="center" width="50" />
          <el-table-column key="receiveInvoiceDate" prop="receiveInvoiceDate" label="*收票日期" align="center" width="160">
            <template v-slot="scope">
              <el-date-picker
                v-if="scope.row.isModify"
                v-model="scope.row.receiveInvoiceDate"
                type="date"
                size="small"
                value-format="x"
                placeholder="选择日期"
                style="width:100%"
                :disabledDate="(date) => {return date.getTime() > new Date().getTime()}"
              />
              <template v-else>
                <div>{{ scope.row.receiveInvoiceDate? parseTime(scope.row.receiveInvoiceDate,'{y}-{m}-{d}'): '-' }}</div>
              </template>
            </template>
          </el-table-column>
          <el-table-column key="invoiceAmount2" prop="invoiceAmount2" label="*收票额" align="center" class="money-column">
            <el-table-column key="invoiceAmount" prop="invoiceAmount" label="小写" align="center" min-width="85">
              <template v-slot="scope">
                <el-input-number
                  v-if="scope.row.isModify"
                  v-show-thousand
                  v-model.number="scope.row.invoiceAmount"
                  :min="0"
                  :max="currentRow.freight-totalAmount"
                  :step="100"
                  :precision="DP.YUAN"
                  placeholder="收票额(元)"
                  controls-position="right"
                  @change="moneyChange(scope.row)"
                />
                <div v-else>{{ scope.row.invoiceAmount && scope.row.invoiceAmount>0? toThousand(scope.row.invoiceAmount): scope.row.invoiceAmount }}</div>
              </template>
            </el-table-column>
            <el-table-column key="invoiceAmount1" prop="invoiceAmount1" label="大写" align="center" width="330" :show-overflow-tooltip="true">
              <template v-slot="scope">
                <div>{{scope.row.invoiceAmount?'('+digitUppercase(scope.row.invoiceAmount)+')':''}}</div>
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
          <el-table-column key="branchCompanyId" prop="branchCompanyId" label="*购方单位" align="center" :show-overflow-tooltip="true">
            <template v-slot="scope">
              <div>{{ scope.row.branchCompanyName }}</div>
            </template>
          </el-table-column>
          <el-table-column key="supplierId" prop="supplierId" label="*销售单位" align="center" :show-overflow-tooltip="true">
            <template v-slot="scope">
              <div>{{ scope.row.supplierName }}</div>
            </template>
          </el-table-column>
          <el-table-column key="invoiceSerialNumber" prop="invoiceSerialNumber" label="*发票号码" align="center" width="120">
            <template v-slot="scope">
              <el-input v-if="scope.row.isModify" v-model.trim="scope.row.invoiceSerialNumber" type="text" placeholder="发票号码" @blur="checkInvoiceNo(scope.row,scope.$index)" maxlength="8"/>
              <span v-else>{{ scope.row.invoiceSerialNumber }}</span>
            </template>
          </el-table-column>
          <el-table-column key="attachments" prop="attachments" label="发票附件" align="center" width="150">
            <template v-slot="scope">
              <upload-btn ref="uploadRef" v-model:files="scope.row.attachments" :file-classify="fileClassifyEnum.CONTRACT_ATT.V" :limit="1" :accept="'.pdf,.jpg,.jpeg,.png'"/>
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
import useMaxHeight from '@compos/use-max-height'
import { digitUppercase } from '@/utils/data-type/number'
import { toThousand } from '@data-type/number'
import { invoiceTypeEnum } from '@enum-ms/finance'
import useTableValidate from '@compos/form/use-table-validate'
import { fileClassifyEnum } from '@enum-ms/file'
import UploadBtn from '@comp/file-upload/UploadBtn'

const formRef = ref()
const detailRef = ref()
const defaultForm = {
  list: []
}

const { CRUD, crud, form } = regForm(defaultForm, formRef)
const totalAmount = inject('totalAmount')
const extraAmount = ref(0)
const invoiceNoArr = ref([])

const props = defineProps({
  existInvoiceNo: {
    type: Array,
    default: () => []
  },
  currentRow: {
    type: Object,
    default: () => {}
  },
  propertyType: {
    type: [Number, String],
    default: undefined
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

const validateTaxRate = (value, row) => {
  if (row.invoiceType !== invoiceTypeEnum.RECEIPT.V) return !!value

  return true
}

// 金额校验
const validateAmount = (value, row) => {
  if (!value) return false
  return true
}

const tableRules = {
  receiveInvoiceDate: [{ required: true, message: '请选择收票日期', trigger: 'change' }],
  invoiceAmount: [{ validator: validateAmount, message: '请选择收票额', trigger: 'change', type: 'number' }],
  taxRate: [{ validator: validateTaxRate, message: '请输入税率', trigger: 'blur' }],
  invoiceType: [{ required: true, message: '请选择发票类型', trigger: 'change' }],
  invoiceSerialNumber: [{ required: true, message: '请输入发票号', trigger: 'blur' }],
  supplierId: [{ required: true, message: '请输入收票单位', trigger: 'blur' }],
  branchCompanyId: [{ required: true, message: '请输入收票单位', trigger: 'blur' }]
}

const { tableValidate, wrongCellMask } = useTableValidate({ rules: tableRules }) // 表格校验

function deleteRow(index) {
  form.list.splice(index, 1)
}

function addRow() {
  form.list.push({
    id: undefined,
    orderId: props.currentRow.id,
    propertyType: props.propertyType,
    invoiceAmount: undefined,
    receiveInvoiceDate: undefined,
    invoiceType: undefined,
    invoiceSerialNumber: undefined,
    taxRate: undefined,
    tax: undefined,
    branchCompanyId: props.currentRow.branchCompanyId,
    branchCompanyName: props.currentRow.branchCompanyName,
    supplierId: props.currentRow.supplierId,
    supplierName: props.currentRow.supplierName,
    dataIndex: form.list.length + 1,
    isModify: true
  })
}

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
  if (extraAmount.value > (props.currentRow.freight - totalAmount.value)) {
    const num = row.invoiceAmount - (extraAmount.value - (props.currentRow.freight - totalAmount.value))
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
  const val = invoiceNoArr.value.find(v => v.dataIndex === row.dataIndex)
  console.log(invoiceNoArr)
  console.log(val)
  if (val) {
    if (row.invoiceSerialNumber) {
      if (val.invoiceSerialNumber === row.invoiceSerialNumber) {
        return
      }
      if (invoiceNoArr.value.findIndex(v => v.invoiceSerialNumber === row.invoiceSerialNumber) > -1) {
        ElMessage({ message: '发票号已存在，请重新填写', type: 'error' })
        row.invoiceSerialNumber = undefined
      } else {
        val.invoiceSerialNumber = row.invoiceSerialNumber
      }
    } else {
      val.invoiceSerialNumber = undefined
    }
  } else {
    if (invoiceNoArr.value.findIndex(v => v.invoiceSerialNumber === row.invoiceSerialNumber) > -1) {
      ElMessage({ message: '发票号已存在，请重新填写', type: 'error' })
      row.invoiceSerialNumber = undefined
    } else {
      invoiceNoArr.value.push({
        invoiceSerialNumber: row.invoiceSerialNumber,
        dataIndex: row.dataIndex
      })
    }
  }
}
CRUD.HOOK.beforeValidateCU = (crud, form) => {
  if (crud.form.list.length <= 0) {
    ElMessage({ message: '请添加收票明细', type: 'error' })
    return false
  }
  const { validResult, dealList } = tableValidate(crud.form.list)
  if (validResult) {
    crud.form.list = dealList
  } else {
    return validResult
  }
  let moneyFlag = true
  crud.form.list.map(row => {
    if (row.invoiceAmount === 0) {
      moneyFlag = false
    }
    row.attachmentIds = row.attachments ? row.attachments.map((v) => v.id) : undefined
  })
  if (!moneyFlag) {
    ElMessage.error('收票金额必须大于0')
    return false
  }
}

</script>
<style lang="scss" scoped>
.add-row-box {
  text-align: center;
  margin-top: 20px;
}
</style>
