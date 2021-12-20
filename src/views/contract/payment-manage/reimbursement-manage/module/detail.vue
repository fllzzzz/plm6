<template>
  <common-drawer
    append-to-body
    v-model="visible"
    top="10vh"
    width="600px"
    :before-close="closeDrawer"
    title="报销信息"
    :wrapper-closable="false"
    size="1300px"
  >
    <template #title>
      <div class="dialog-title">
        <span style="margin-right:5px;">报销信息</span>
        <common-button v-if="collectionInfo.confirmStatus" size="mini" :type="collectionInfo.confirmStatus == reimbursementTypeEnum.ENUM.REJECT.V?'info':(collectionInfo.confirmStatus == reimbursementTypeEnum.ENUM.PASS.V?'success':'warning')">
          {{ collectionInfo.confirmStatus == reimbursementTypeEnum.ENUM.REJECT.V?'已退回':(collectionInfo.confirmStatus==reimbursementTypeEnum.ENUM.PASS.V?'已确认':'确认中') }}
        </common-button>
        <span style="position:absolute;right:20px;">
          <template v-if="collectionInfo.confirmStatus">
            <template v-if="!isModify">
              <common-button v-if="collectionInfo.confirmStatus == reimbursementTypeEnum.ENUM.AUDITING.V && type ==='audit'" size="small" type="info" @click="onSubmit(reimbursementTypeEnum.ENUM.REJECT.V)">退回</common-button>
              <common-button v-if="collectionInfo.confirmStatus == reimbursementTypeEnum.ENUM.AUDITING.V && type ==='audit'" size="small" type="success" @click="onSubmit(reimbursementTypeEnum.ENUM.PASS.V)">确定</common-button>
              <common-button size="small" type="primary" @click="modifyInfo" v-if="collectionInfo.confirmStatus == reimbursementTypeEnum.ENUM.REJECT.V && type === 'detail'">重新编辑</common-button>
            </template>
            <template v-else>
              <common-button slot="reference" type="primary" size="small" @click="onSubmit">提交</common-button>
            </template>
          </template>
          <common-button size="small"  @click="closeDrawer">关闭</common-button>
        </span>
      </div>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="140px">
        <div class="form-row" style="display: flex">
          <el-form-item label="项目" prop="projectId">
            <div style="width:360px;">
              <project-cascader v-if="isModify" v-model="form.projectId" style="width: 320px" class="filter-item" />
              <span v-else>{{ collectionInfo.projectName }}</span>
            </div>
          </el-form-item>
          <el-form-item label="申请金额(元)" prop="applyAmount">
            <div style="width:360px;">
              <el-input v-if="isModify" v-model="form.applyAmount" type="text" placeholder="申请金额" style="width: 320px" disabled />
              <span v-else>{{ collectionInfo.applyAmount }}</span>
              <span v-if="upperYuan" style="size:12px;">{{ upperYuan }}</span>
            </div>
          </el-form-item>
        </div>
        <div class="form-row" style="display: flex">
          <el-form-item label="业务类型" prop="businessType">
            <div style="width:360px;">
              <span>{{ contractInfo.businessTypeName }}</span>
            </div>
          </el-form-item>
          <el-form-item label="申请人" prop="applyUserId">
            <div style="width:360px;">
              <user-dept-cascader
                v-if="isModify"
                ref="applyRef"
                v-model="form.applyUserId"
                filterable
                clearable
                show-all-levels
                style="width: 320px"
                placeholder="申请人"
                @change="getDept"
              />
              <span v-else>{{ collectionInfo.applyUserName }}</span>
            </div>
          </el-form-item>
        </div>
        <div class="form-row" style="display: flex">
          <el-form-item label="收款人" prop="collectionUserId">
            <div style="width:360px;">
              <user-dept-cascader
                v-model="form.collectionUserId"
                ref="collectionRef"
                filterable
                clearable
                show-all-levels
                style="width: 320px"
                placeholder="收款人"
                @change="getCollectionChange"
              />
            </div>
          </el-form-item>
          <el-form-item label="申请日期" prop="applyDate">
            <div style="width:360px;">
              <el-date-picker v-if="isModify" v-model="form.applyDate" type="date" value-format="x" placeholder="申请日期" style="width: 320px" />
              <template v-else>
                <span v-parse-time="'{y}-{m}-{d}'">{{ collectionInfo.applyDate }}</span>
              </template>
            </div>
          </el-form-item>
        </div>
        <div class="form-row" style="display: flex">
          <el-form-item label="收款开户行" prop="collectionDepositBank">
            <div style="width:360px;">
              <el-input v-model="form.collectionDepositBank" type="text" placeholder="收款开户行" style="width: 320px" />
            </div>
          </el-form-item>
          <el-form-item label="收款账号" prop="collectionBankAccount">
            <div style="width:360px;">
              <el-input v-model="form.collectionBankAccount" type="text" placeholder="收款账号" style="width: 320px" />
            </div>
          </el-form-item>
        </div>
        <div class="form-row" style="display: flex" v-if="collectionInfo.confirmStatus == reimbursementTypeEnum.ENUM.AUDITING.V && type ==='audit'">
          <el-form-item label="付款单位" prop="collectionBankAccount">
            <div style="width:360px;">
              <common-select
                v-model="form.paymentUnitId"
                :options="contractInfo.companyBankAccountList"
                :type="'other'"
                :dataStructure="typeProp"
                size="small"
                clearable
                class="filter-item"
                placeholder="付款单位"
                style="width:320px"
                @change="orderCompanyChange"
              />
            </div>
          </el-form-item>
          <el-form-item label="实付金额(元)" prop="actuallyPayAmount" v-if="collectionInfo.confirmStatus == reimbursementTypeEnum.ENUM.AUDITING.V && type ==='audit'">
            <div style="width:360px;">
              <el-input v-model="form.actuallyPayAmount" type="text" placeholder="实付金额(元)" style="width: 320px" disabled />
            </div>
          </el-form-item>
        </div>
        <div class="form-row" style="display: flex">
          <el-form-item label="付款开户行" prop="paymentDepositBank" v-if="collectionInfo.confirmStatus == reimbursementTypeEnum.ENUM.AUDITING.V && type ==='audit'">
            <div style="width:360px;">
              <el-input v-model="form.paymentDepositBank" type="text" placeholder="付款开户行" style="width: 320px" />
            </div>
          </el-form-item>
          <el-form-item label="付款账号" prop="paymentBankAccount" v-if="collectionInfo.confirmStatus == reimbursementTypeEnum.ENUM.AUDITING.V && type ==='audit'">
            <div style="width:360px;">
              <el-input v-model="form.paymentBankAccount" type="text" placeholder="付款账号" style="width: 320px" />
            </div>
          </el-form-item>
        </div>
        <el-form-item label="备注" prop="remark">
          <div style="width:360px;">
            <el-input
              v-if="isModify"
              v-model="form.remark"
              type="textarea"
              :autosize="{ minRows: 6, maxRows: 8 }"
              :maxLength="500"
              placeholder="可填写备注"
              style="max-width: 500px"
            />
            <span v-else>{{ collectionInfo.remark }}</span> 
          </div>
        </el-form-item>
        <common-table
          ref="detailRef"
          border
          :data="isModify?form.detailList:collectionInfo.detailList"
          :max-height="300"
          style="width: 100%"
          class="table-form"
          :cell-class-name="wrongCellMask"
        >
          <el-table-column label="序号" type="index" align="center" width="50" />
          <el-table-column prop="choseId" label="报销种类" align="center" min-width="250">
            <template v-slot="scope">
              <expense ref="expenseRef" v-model="scope.row.choseId" @change="expenseChange" :data-index="scope.$index" />
            </template>
          </el-table-column>
          <el-table-column prop="applyAmount" label="申请金额（元）" align="center" min-width="120">
            <template v-slot="scope">
              <el-input-number
                v-model="scope.row.applyAmount"
                :min="1"
                :max="99999999999"
                :step="10000"
                :precision="DP.YUAN"
                size="small"
                controls-position="right"
                placeholder="申请金额"
                style="width: 100%; max-width: 200px"
                @change="applyAmountChange(scope.row.applyAmount, scope.$index)"
              />
            </template>
          </el-table-column>
          <el-table-column prop="invoiceType" label="发票类型" align="center" min-width="160">
            <template v-slot="scope">
              <common-select
                v-model="scope.row.invoiceType"
                :options="invoiceTypeEnum.ENUM"
                type="enum"
                size="small"
                clearable
                class="filter-item"
                placeholder="发票类型"
                style="width: 130px"
              />
            </template>
          </el-table-column>
          <el-table-column prop="invoiceNo" label="发票号码" align="center" min-width="150">
            <template v-slot="scope">
              <el-input v-model="scope.row.invoiceNo" type="text" placeholder="发票号码" style="width: 120px" />
            </template>
          </el-table-column>
          <el-table-column prop="invoiceAmount" label="发票面额（元）" align="center" min-width="120">
            <template v-slot="scope">
              <el-input-number
                v-model="scope.row.invoiceAmount"
                :min="scope.row.applyAmount ? scope.row.applyAmount : 1"
                :max="99999999999"
                :step="10000"
                :precision="DP.YUAN"
                size="small"
                controls-position="right"
                placeholder="发票面额"
                style="width: 100%; max-width: 200px"
                @change="invoiceTypeChange(scope.$index)"
              />
            </template>
          </el-table-column>
          <el-table-column prop="taxRate" label="税率" align="center" min-width="80">
            <template v-slot="scope">
              <div v-if="scope.row.invoiceType === invoiceTypeEnum.ENUM.SPECIAL.V">
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
                  @change="invoiceTypeChange(scope.$index)"
                />%
              </div>
            </template>
          </el-table-column>
          <el-table-column prop="inputTax" label="进项税额(元)" align="center" min-width="130">
            <template v-slot="scope">
              <el-input
                v-model="scope.row.inputTax"
                type="text"
                placeholder="先输入税率"
                style="width: 100px"
                disabled
                v-if="scope.row.invoiceType === invoiceTypeEnum.ENUM.SPECIAL.V"
              />
            </template>
          </el-table-column>
          <el-table-column prop="actuallyPayAmount" label="实付金额(元)" align="center" min-width="160" v-if="collectionInfo.confirmStatus == reimbursementTypeEnum.ENUM.AUDITING.V && type ==='audit'">
            <template v-slot="scope">
              <el-input-number
                v-model="scope.row.actuallyPayAmount"
                :min="1"
                :max="99999999999"
                :step="10000"
                :precision="DP.YUAN"
                size="small"
                controls-position="right"
                placeholder="实付金额"
                style="width: 100%; max-width: 200px"
                @change="actuallyPayAmountChange()"
              />
            </template>
          </el-table-column>
          <el-table-column label="操作" align="center" v-if="isModify">
            <template v-slot="scope">
              <common-button size="small" class="el-icon-delete" type="danger" @click="deleteRow(scope.$index)"/>
            </template>
          </el-table-column>
        </common-table>
        <div class="add-row-box">
          <common-button size="mini" icon="el-icon-circle-plus-outline" type="warning" style="margin-right: 15px" @click="addRow()" v-if="isModify"
            >继续添加</common-button
          >
        </div>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, watch, computed, defineProps, defineEmits } from 'vue'
import projectCascader from '@comp-base/project-cascader'
import { DP } from '@/settings/config'
import { contractCollectionInfo } from '@/api/contract/collection-and-invoice/collection'
import useWatchFormValidate from '@compos/form/use-watch-form-validate'
import useVisible from '@compos/use-visible'
import { invoiceTypeEnum } from '@enum-ms/finance'
import { reimbursementTypeEnum, businessTypeEnum } from '@enum-ms/contract'
import { toThousand, digitUppercase } from '@/utils/data-type/number'
import { editStatus } from '@/api/contract/collection-and-invoice/invoice'
import { ElNotification } from 'element-plus'
import UploadBtn from '@/components/file-upload/UploadBtn'
import useTableValidate from '@compos/form/use-table-validate'
import userDeptCascader from '@comp-base/user-dept-cascader.vue'
import { isNotBlank } from '@data-type/index'
import Expense from './expense'

const formRef = ref()
const defaultForm = {
  id: undefined,
  applyAmount: undefined,
  applyDate: undefined,
  applyDepartId: undefined,
  applyUserId: undefined,
  collectionBankAccount: undefined,
  collectionDepositBank: undefined,
  collectionUserId: undefined,
  detailList: [],
  projectId: undefined,
  remark: undefined,
}

const props = defineProps({
  collectionInfo: {
    type: Object,
    default: () => {}
  },
  modelValue: {
    type: Boolean,
    require: true
  },
  type: {
    type: String,
    require: true
  }
})

const typeProp = { key: 'companyId', label: 'companyName', value: 'companyId' }
const form = ref(JSON.parse(JSON.stringify(defaultForm)))
const contractInfo = ref({})
const isModify = ref(false)
const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

watch(
  () => props.collectionInfo.projectId,
  (val) => {
    if (val) {
      getContractInfo(val)
    }
  },
  { deep: true, immediate: true }
)

watch(
  () => props.collectionInfo.id,
  (val) => {
    if (val) {
      resetForm()
    }
  },
  { deep: true, immediate: true }
)

const rules = {
  projectId: [{ required: true, message: '请选择项目', trigger: 'change' }],
  applyAmount: [{ required: true, message: '请输入申请金额', trigger: 'blur' }],
  collectionUserId: [{ required: true, message: '请选择收款人', trigger: 'change' }],
  applyUserId: [{ required: true, message: '请选择申请人', trigger: 'change' }],
  applyDate: [{ required: true, message: '请选择申请日期', trigger: 'change' }],
}

const tableRules = {
  choseId: [{ required: true, message: '请选择报销种类', trigger: 'change' }],
  applyAmount: [{ required: true, message: '请输入申请金额', trigger: 'change', type: 'number' }],
}
const { tableValidate, wrongCellMask } = useTableValidate({ rules: tableRules })


function modifyInfo(){
  resetForm()
  isModify.value = true
}

function closeDrawer(){
  isModify.value = false
  handleClose()
}

function resetForm() {
  if (formRef.value) {
    formRef.value.resetFields()
  }
  let DataValue = JSON.parse(JSON.stringify(props.collectionInfo))
  DataValue.applyDate = String(DataValue.applyDate)
  form.value = DataValue
  useWatchFormValidate(formRef, form)
}

async function getContractInfo(id) {
  let data = {}
  try {
    data = await contractCollectionInfo({ projectId: id })
  } catch (e) {
    console.log('获取合同信息', e)
  } finally {
    contractInfo.value = data
    contractInfo.value.businessTypeName = contractInfo.value.businessType ? businessTypeEnum.VL[contractInfo.value.businessType] : ''
  }
}

function orderCompanyChange(val){
  if(val){
    const orderVal = contractInfo.value.companyBankAccountList.find(v=>v.companyId===val)
    form.value.paymentBankAccount = orderVal.account
    form.value.paymentDepositBank = orderVal.depositBank
    form.value.paymentUnit = orderVal.companyName
  } else {
    form.value.paymentBankAccount = ''
    form.value.paymentDepositBank = ''
    form.value.paymentUnit = ''
  }
}

function getCollectionChange() {
  const val = collectionRef.value.getNodeInfo()
  if (isNotBlank(val)) {
    form.value.collectionUserName = val.label
  } else {
    form.value.collectionUserName = undefined
  }
}

function getDept() {
  if (form.value.applyUserId) {
    const val = applyRef.value.getNodeInfo()
    form.value.applyDepartId = val.parentDeptId
    form.value.applyDepartName = val.parentDeptName
    form.value.applyUserName = val.label
    if (!form.value.collectionUserId) {
      form.value.collectionUserId = form.value.applyUserId
    }
  } else {
    form.value.applyDepartId = undefined
    form.value.applyDepartName = undefined
    form.value.applyUserName = undefined
  }
}

function applyAmountChange(amount, index) {
  if (amount) {
    form.detailList[index].invoiceAmount = amount
  }
  if (form.detailList.length > 0) {
    let val = 0
    form.value.detailList.forEach((v) => {
      if (v.applyAmount > 0) {
        val += v.applyAmount
      }
    })
    form.value.applyAmount = val
  } else {
    form.value.applyAmount = undefined
  }
}

function actuallyPayAmountChange(){
  if (props.collectionInfo.detailList.length > 0) {
    let val = 0
    props.collectionInfo.detailList.forEach((v) => {
      if (v.applyAmount > 0) {
        val += v.applyAmount
      }
    })
    props.collectionInfo.actuallyPayAmount = val
  } else {
    props.collectionInfo.actuallyPayAmount = undefined
  }
}

const upperYuan = computed(() => {
  return form.value.applyAmount ? digitUppercase(form.value.applyAmount) : ''
})

function expenseChange(val) {
  if (val) {
    if (val.type === 1) {
      form.value.detailList[val.dataIndex].expenseTypeId = val.id
      form.value.detailList[val.dataIndex].dictionaryId = undefined
    } else {
      form.value.detailList[val.dataIndex].expenseTypeId = val.parentId
      form.value.detailList[val.dataIndex].dictionaryId = val.id
    }
  }
}

function invoiceTypeChange(index) {
  if (form.value.detailList[index].taxRate && form.value.detailList[index].invoiceAmount) {
    form.value.detailList[index].inputTax = ((form.value.detailList[index].invoiceAmount * form.value.detailList[index].taxRate) / 100).toFixed(DP.YUAN)
  } else {
    form.value.detailList[index].inputTax = undefined
  }
}

function deleteRow(index) {
  form.value.detailList.splice(index, 1)
  applyAmountChange()
}

function addRow() {
  form.value.detailList.push({
    applyAmount: undefined,
    choseId: undefined,
    dictionaryId: undefined,
    expenseTypeId: undefined,
    inputTax: undefined,
    invoiceAmount: undefined,
    invoiceNo: undefined,
    invoiceType: undefined,
    taxRate: undefined,
    verify:{}
  })
}

async function onSubmit(val){
    const { validResult, dealList } = tableValidate(form.value.detailList)
    if (validResult) {
      form.value.detailList = dealList
    } else {
      return validResult
    }
    try{
      if(props.type === 'detail'){
        const valid = await formRef.value.validate()
        if (valid) {
          //修改
        }
      }else{
        await editStatus(props.collectionInfo.id,val)
        ElNotification({ title: '提交成功', type: 'success' })
        emit('success')
      }
    }catch(e){

    }finally{
      closeDrawer()
    }
}

</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
.add-row-box {
  text-align: center;
  margin-top: 20px;
}
</style>
