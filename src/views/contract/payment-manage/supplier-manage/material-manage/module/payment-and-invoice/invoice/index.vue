<template>
  <div class="app-container">
    <!--表格渲染-->
    <div>
      <common-button type="primary" size="mini" @click="crud.toAdd" style="margin-right:10px;" v-permission="permission.add">添加</common-button>
      <el-tag type="success" size="medium" v-if="currentRow.amount">{{`合同额:${toThousand(currentRow.amount,decimalPrecision.contract)}`}}</el-tag>
      <el-tag type="success" size="medium" v-if="currentRow.settlementAmount" style="margin-left:5px;">{{`结算额:${toThousand(currentRow.settlementAmount,decimalPrecision.contract)}`}}</el-tag>
    </div>
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%;margin-top:10px;"
      class="collection-table"
      :cell-class-name="wrongCellMask"
      return-source-data
      :showEmptySymbol="false"
      show-summary
      :summary-method="getSummaries"
      :stripe="false"
    >
      <el-table-column prop="index" label="序号" align="center" width="50" type="index" />
      <el-table-column key="receiveInvoiceDate" prop="receiveInvoiceDate" label="收票日期" align="center" width="160">
        <template v-slot="scope">
          <el-date-picker
            v-if="scope.row.isModify"
            v-model="scope.row.receiveInvoiceDate"
            type="date"
            size="small"
            value-format="x"
            placeholder="选择日期"
            :disabledDate="(date) => {return date.getTime() > new Date().getTime()}"
            style="width:100%"
          />
          <template v-else>
            <div>{{ scope.row.receiveInvoiceDate? parseTime(scope.row.receiveInvoiceDate,'{y}-{m}-{d}'): '-' }}</div>
          </template>
        </template>
      </el-table-column>
      <el-table-column key="invoiceAmount1" prop="invoiceAmount1" label="收票额" align="center" class="money-column">
        <el-table-column key="invoiceAmount" prop="invoiceAmount" label="小写" align="center" min-width="110">
          <template v-slot="scope">
            <el-input-number
              v-if="scope.row.isModify"
              v-show-thousand
              v-model.number="scope.row.invoiceAmount"
              :min="-9999999999"
              :max="currentRow.settlementAmount?currentRow.settlementAmount:9999999999"
              :step="100"
              :precision="decimalPrecision.contract"
              placeholder="收票额(元)"
              controls-position="right"
              @change="moneyChange(scope.row)"
            />
            <div v-else>{{ isNotBlank(scope.row.invoiceAmount) ? toThousand(scope.row.invoiceAmount,decimalPrecision.contract): '-' }}</div>
          </template>
        </el-table-column>
        <el-table-column key="invoiceAmount2" prop="invoiceAmount2" label="大写" align="center" width="330" :show-overflow-tooltip="true">
          <template v-slot="scope">
            <div>{{scope.row.invoiceAmount?digitUppercase(scope.row.invoiceAmount):''}}</div>
          </template>
        </el-table-column>
      </el-table-column>
      <el-table-column key="attachments" prop="attachments" label="附件" align="center" width="150" :show-overflow-tooltip="true">
        <template #header>
          <el-tooltip effect="light" :content="`双击可预览附件`" placement="top">
            <div>
              <span>附件</span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template>
        <template v-slot="scope">
          <upload-btn ref="uploadRef" v-if="scope.row.isModify" v-model:files="scope.row.files" :file-classify="fileClassifyEnum.CONTRACT_ATT.V" :limit="1" :accept="'.pdf,.jpg,.jpeg,.png'"/>
          <template v-if="scope.row.attachments && scope.row.attachments.length>0 && !scope.row.files">
            <div v-for="item in scope.row.attachments" :key="item.id">
              <div style="cursor:pointer;" @dblclick="attachmentView(item)">{{item.name}}</div>
            </div>
          </template>
        </template>
      </el-table-column>
      <el-table-column key="invoiceType" prop="invoiceType" label="发票类型" align="center" width="120" >
        <template v-slot="scope">
          <div>{{ scope.row.invoiceType? invoiceTypeEnum.VL[scope.row.invoiceType]: '' }}</div>
        </template>
      </el-table-column>
      <el-table-column key="taxRate" prop="taxRate" label="税率" align="center" width="150">
        <template v-slot="scope">
          <div v-if="scope.row.invoiceType !== invoiceTypeEnum.RECEIPT.V && scope.row.isModify">
            <div style="display:flex;">
              <div style="padding-right:5px;">
                <el-checkbox v-model="scope.row.checked" label="免税" size="large" @change="checkChange(scope.row)" />
              </div>
              <el-input-number
                v-model="scope.row.taxRate"
                :step="1"
                :min="0"
                :max="100"
                :precision="0"
                :controls="false"
                controls-position="right"
                class="input-underline"
                style="width: 100px; text-align: center"
                placeholder="0-100"
                :disabled="scope.row.checked"
                @change="taxMoney(scope.row)"
              />
              <span style="line-height:40px;">%</span>
            </div>
          </div>
          <div v-else>{{ scope.row.checked && scope.row.invoiceType?'免税':(isNotBlank(scope.row.taxRate)? scope.row.taxRate+'%': '-') }}</div>
        </template>
      </el-table-column>
      <!-- <el-table-column key="taxRate" prop="taxRate" label="税率" align="center" width="80">
        <template v-slot="scope">
          <div v-if="scope.row.invoiceType !== invoiceTypeEnum.RECEIPT.V">{{ scope.row.taxRate? scope.row.taxRate+'%': '' }}</div>
        </template>
      </el-table-column> -->
      <el-table-column key="branchCompanyId" prop="branchCompanyId" label="购方单位" align="center" min-width="120" :show-overflow-tooltip="true">
        <template v-slot="scope">
          <div>{{ scope.row.branchCompanyName }}</div>
        </template>
      </el-table-column>
      <el-table-column key="supplierId" prop="supplierId" label="销售单位" align="center" min-width="120" :show-overflow-tooltip="true">
        <template v-slot="scope">
          <div>{{ scope.row.supplierName }}</div>
        </template>
      </el-table-column>
      <el-table-column prop="invoiceSerialNumber" label="发票号码" align="center" min-width="150">
        <template v-slot="scope">
          <el-input v-if="scope.row.isModify" v-model.trim="scope.row.invoiceSerialNumber" type="text" placeholder="发票号码" style="width: 100%;" maxlength="20"/>
          <span v-else>{{ scope.row.invoiceSerialNumber  }}</span>
        </template>
      </el-table-column>
      <el-table-column key="applyUserName" prop="applyUserName" label="办理人" align="center" width="100px">
        <template v-slot="scope">
          <div>{{ scope.row.applyUserName }}</div>
        </template>
      </el-table-column>
      <el-table-column key="auditUserName" prop="auditUserName" label="审核人" align="center" width="100px">
        <template v-slot="scope">
          <div>{{ scope.row.auditUserName }}</div>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column
        v-if="checkPermission([ ...permission.edit,...permission.audit])"
        label="操作"
        width="190px"
        align="center"
      >
        <template v-slot="scope">
          <template v-if="!scope.row.isModify">
            <common-button icon="el-icon-edit" type="primary" size="mini" @click="modifyRow(scope.row)" v-if="scope.row.auditStatus===auditTypeEnum.AUDITING.V && checkPermission(permission.edit)"/>
            <el-popconfirm
              confirm-button-text="确定"
              cancel-button-text="取消"
              title="确定删除吗?"
              @confirm="rowDelete(scope.row)"
              v-if="scope.row.auditStatus===auditTypeEnum.AUDITING.V && checkPermission(permission.del)"
            >
              <template #reference>
                <common-button icon="el-icon-delete" type="danger" size="mini" />
              </template>
            </el-popconfirm>
            <el-popconfirm
              confirm-button-text="确定"
              cancel-button-text="取消"
              title="确定通过吗?"
              @confirm="passConfirm(scope.row)"
              v-if="scope.row.auditStatus===auditTypeEnum.AUDITING.V && checkPermission(permission.audit)"
            >
              <template #reference>
                <common-button type="success" size="mini" >通过</common-button>
              </template>
            </el-popconfirm>
            <el-tag type="success" v-if="scope.row.auditStatus===auditTypeEnum.PASS.V" class="pass-tag">已复核</el-tag>
          </template>
          <template v-else>
            <el-popconfirm
              confirm-button-text="确定"
              cancel-button-text="取消"
              title="确定取消吗?"
              @confirm="rowCancel(scope.row,scope.$index)"
            >
              <template #reference>
                <common-button type="primary" size="mini">取消</common-button>
              </template>
            </el-popconfirm>
            <common-button type="info" plain size="mini" @click="rowSubmit(scope.row)">保存</common-button>
          </template>
        </template>
      </el-table-column>
    </common-table>
  <!--分页组件-->
  <pagination />
  <mForm :existInvoiceNo="invoiceNoArr" :currentRow="currentRow" :propertyType="propertyType"/>
  <showPdfAndImg v-if="pdfShow" :isVisible="pdfShow" :showType="'attachment'" :id="currentId" @close="pdfShow=false"/>
  </div>
</template>

<script setup>
import crudApi, { editStatus } from '@/api/contract/supplier-manage/pay-invoice/invoice'
import { ref, defineProps, watch, provide, defineEmits, nextTick } from 'vue'

import { tableSummary } from '@/utils/el-extra'
import checkPermission from '@/utils/system/check-permission'
import { auditTypeEnum, supplierPayTypeEnum } from '@enum-ms/contract'
import { invoiceTypeEnum } from '@enum-ms/finance'
import { parseTime } from '@/utils/date'
import { toThousand } from '@data-type/number'
import { digitUppercase } from '@/utils/data-type/number'
import { validate } from '@compos/form/use-table-validate'
import { ElMessage } from 'element-plus'
import { contractSupplierMaterialPM } from '@/page-permission/contract'
import { fileClassifyEnum } from '@enum-ms/file'
import useDecimalPrecision from '@compos/store/use-decimal-precision'
import { isNotBlank } from '@data-type/index'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import UploadBtn from '@comp/file-upload/UploadBtn'
import showPdfAndImg from '@comp-base/show-pdf-and-img.vue'
import mForm from './form'

const { decimalPrecision } = useDecimalPrecision()

const permission = contractSupplierMaterialPM.invoice
const emit = defineEmits(['success'])
const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}

const props = defineProps({
  currentRow: {
    type: Object,
    default: () => {}
  },
  visibleValue: {
    type: Boolean,
    default: false
  },
  propertyType: {
    type: [Number, String],
    default: undefined
  }
})
const tableRef = ref()
const originRow = ref({})
const totalAmount = ref(0)
const invoiceNoArr = ref([])
const pdfShow = ref(false)
const currentId = ref()
provide('totalAmount', totalAmount)
const { crud, CRUD } = useCRUD(
  {
    title: '收票填报',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    requiredQuery: ['orderId'],
    invisibleColumns: ['haveInvoiceAmount', 'collectionMode', 'invoiceType', 'collectionDepositBank', 'collectionBankAccount', 'paymentBankAccount', 'paymentDepositBank', 'auditorName', 'auditTime'],
    hasPagination: true
  },
  tableRef
)

const validateTaxRate = (value, row) => {
  if (row.invoiceType !== invoiceTypeEnum.RECEIPT.V && !row.checked) return isNotBlank(value)
  return true
}

// 金额校验
const validateAmount = (value, row) => {
  if (!isNotBlank(value)) return false
  return true
}

const tableRules = {
  receiveInvoiceDate: [{ required: true, message: '请选择收票日期', trigger: 'change' }],
  invoiceAmount: [{ validator: validateAmount, message: '请选择收票额', trigger: 'change', type: 'number' }],
  taxRate: [{ validator: validateTaxRate, message: '请输入税率', trigger: 'blur' }],
  invoiceType: [{ required: true, message: '请选择发票类型', trigger: 'change' }],
  invoiceSerialNumber: [{ required: true, message: '请输入发票号', trigger: 'blur' }]
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

const { maxHeight } = useMaxHeight({
  wrapperBox: '.pay-invoice',
  paginate: true,
  extraHeight: 40
})

watch(
  () => props.visibleValue,
  (val) => {
    if (val) {
      crud.query.orderId = props.currentRow.id
      crud.query.propertyType = supplierPayTypeEnum.PURCHASE.V
      crud.toQuery()
    }
  },
  { deep: true, immediate: true }
)

function moneyChange(row) {
  if (props.currentRow.settlementAmount) {
    totalAmount.value = 0
    crud.data.map(v => {
      if (v.invoiceAmount) {
        totalAmount.value += v.invoiceAmount
      }
    })
    if (totalAmount.value > props.currentRow.settlementAmount) {
      const num = row.invoiceAmount - (totalAmount.value - props.currentRow.settlementAmount)
      // 解决修改失效
      nextTick(() => {
        row.invoiceAmount = num || 0
        totalAmount.value = 0
        crud.data.map(v => {
          if (v.invoiceAmount) {
            totalAmount.value += v.invoiceAmount
          }
        })
      })
    }
  }
  taxMoney(row)
}

function checkChange(row) {
  row.boolIncludeTax = !row.checked
  row.taxRate = row.checked ? 0 : row.originRate
  taxMoney(row)
}

function taxMoney(row) {
  row.tax = 0
  if (isNotBlank(row.invoiceAmount) && row.taxRate) {
    row.tax = row.invoiceAmount * row.taxRate / 100
  }
}
// function checkInvoiceNo(row) {
//   if (row.invoiceSerialNumber) {
//     const val = invoiceNoArr.value.find(v => v.dataIndex === row.dataIndex)
//     if (invoiceNoArr.value.findIndex(v => v.invoiceSerialNumber === row.invoiceSerialNumber) > -1) {
//       ElMessage({ message: '发票号已存在，请重新填写', type: 'error' })
//       row.invoiceSerialNumber = undefined
//       if (val) {
//         val.invoiceSerialNumber = undefined
//       }
//     } else {
//       if (val) {
//         val.invoiceSerialNumber = row.invoiceSerialNumber
//       } else {
//         invoiceNoArr.value.push({
//           invoiceSerialNumber: row.invoiceSerialNumber,
//           dataIndex: row.dataIndex
//         })
//       }
//     }
//   }
// }

async function passConfirm(row) {
  try {
    await editStatus({ id: row.id, auditStatus: auditTypeEnum.PASS.V })
    crud.notify(`审核成功`, CRUD.NOTIFICATION_TYPE.SUCCESS)
    crud.toQuery()
    emit('success')
  } catch (e) {
    console.log('审核失败', e)
  }
}

function modifyRow(row) {
  originRow.value = JSON.parse(JSON.stringify(row))
  row.isModify = true
  row.receiveInvoiceDate = String(row.receiveInvoiceDate)
}

async function rowDelete(row) {
  try {
    await crudApi.del(row.id)
    crud.notify(`删除成功`, CRUD.NOTIFICATION_TYPE.SUCCESS)
    crud.toQuery()
    emit('success')
  } catch (e) {
    console.log(`删除失败`, e)
  }
}
function rowCancel(row) {
  row.isModify = false
  if (row.id) {
    row = Object.assign(row, JSON.parse(JSON.stringify(originRow.value)))
  } else {
    const index = crud.data.findIndex(v => v.dataIndex === row.dataIndex)
    crud.data.splice(index, 1)
  }
}

async function rowSubmit(row) {
  if (row.invoiceAmount === 0) {
    ElMessage.error('收票额必须大于0')
    return
  }
  const rules = tableRules
  let flag = true
  row.verify = {}
  for (const rule in rules) {
    row.verify[rule] = validate(rule, rules[rule], row)
    if (!row.verify[rule]) {
      flag = false
    }
  }
  if (!flag) {
    ElMessage.error('请填写表格中标红数据')
    return
  }
  const messageName = row.id ? '修改' : '新增'
  try {
    if (row.id) {
      row.attachmentIds = row.files ? row.files.map((v) => v.id) : (row.attachments ? row.attachments.map((v) => v.id) : undefined)
      await crudApi.edit(row)
    } else {
      await crudApi.add(row)
    }
    crud.notify(`${messageName}成功`, CRUD.NOTIFICATION_TYPE.SUCCESS)
    crud.toQuery()
  } catch (e) {
    console.log(messageName, e)
  }
}

// 预览附件
function attachmentView(item) {
  currentId.value = item.id
  pdfShow.value = true
}

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: [['invoiceAmount', decimalPrecision.value.contract]],
    toThousandFields: ['invoiceAmount']
  })
}

CRUD.HOOK.handleRefresh = (crud, data) => {
  totalAmount.value = 0
  invoiceNoArr.value = []
  data.data.content.map(v => {
    v.dataIndex = v.id + 'id'
    if (v.invoiceAmount) {
      totalAmount.value += v.invoiceAmount
    }
    if (v.invoiceSerialNumber) {
      invoiceNoArr.value.push({
        invoiceSerialNumber: String(v.invoiceSerialNumber),
        dataIndex: v.dataIndex
      })
    }
    v.checked = !v.boolIncludeTax
    v.originRate = v.taxRate
  })
}

CRUD.HOOK.afterAddSuccess = () => {
  emit('success')
}
</script>

<style lang="scss" scoped>
::v-deep(.abnormal-row) {
  background: #e8f4ff;
}
::v-deep(.hidden-select) {
  td:nth-child(1){
    .cell{
      opacity:0;
    }
  }
}
$font-size: 1.5em;
.child {
  width: $font-size;
  height: $font-size;
  display: inline-block;
  border: 1px solid;
  border-radius: 50%;
  line-height: $font-size;
}
.collection-table{
  ::v-deep(.el-select .el-input__inner){
    padding-left:2px;
    padding-right:5px;
  }
  ::v-deep(.el-input-number .el-input__inner, .el-input__inner) {
    text-align: left;
    padding:0 5px;
  }
  ::v-deep(.el-table .cell){
    padding-left:2px;
    padding-right:2px;
  }
}

::v-deep(.pass-tag){
  padding:0 50px;
}
</style>
