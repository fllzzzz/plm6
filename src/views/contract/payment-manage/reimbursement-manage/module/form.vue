<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    :wrapper-closable="false"
    size="1200px"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="140px">
        <div class="form-row" style="display: flex">
          <el-form-item label="项目" prop="projectId">
            <project-cascader v-model="form.projectId" style="width: 320px" class="filter-item" />
          </el-form-item>
          <el-form-item label="申请金额(元)" prop="applyAmount">
            <el-input v-model="form.applyAmount" type="text" placeholder="申请金额" style="width: 320px" disabled />
            <span v-if="upperYuan" style="size:12px;">{{ upperYuan }}</span>
          </el-form-item>
        </div>
        <div class="form-row" style="display: flex">
          <el-form-item label="业务类型" prop="businessType">
            <el-input v-model="contractInfo.businessTypeName" type="text" placeholder="业务类型" style="width: 320px" disabled />
          </el-form-item>
          <el-form-item label="申请人" prop="applyUserId">
            <user-dept-cascader
              ref="applyRef"
              v-model="form.applyUserId"
              filterable
              clearable
              show-all-levels
              style="width: 320px"
              placeholder="申请人"
              @change="getDept"
            />
          </el-form-item>
        </div>
        <div class="form-row" style="display: flex">
          <el-form-item label="收款人" prop="collectionUserId">
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
          </el-form-item>
          <el-form-item label="申请日期" prop="applyDate">
            <el-date-picker v-model="form.applyDate" type="date" value-format="x" placeholder="申请日期" style="width: 320px" />
          </el-form-item>
        </div>
        <div class="form-row" style="display: flex">
          <el-form-item label="收款开户行" prop="collectionDepositBank">
            <el-input v-model="form.collectionDepositBank" type="text" placeholder="收款开户行" style="width: 320px" />
          </el-form-item>
          <el-form-item label="收款账号" prop="collectionBankAccount">
            <el-input v-model="form.collectionBankAccount" type="text" placeholder="收款账号" style="width: 320px" />
          </el-form-item>
        </div>
        <el-form-item label="备注" prop="remark">
          <el-input
            v-model="form.remark"
            type="textarea"
            :autosize="{ minRows: 6, maxRows: 8 }"
            :maxlength="200"
            show-word-limit
            placeholder="可填写备注"
            style="max-width: 500px"
          />
        </el-form-item>
        <common-table
          ref="detailRef"
          border
          :data="form.detailList"
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
                @change="applyAmountChange(scope.row.applyAmount,scope.$index)"
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
            >继续添加</common-button
          >
        </div>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, computed, watch } from 'vue'
import { regForm } from '@compos/use-crud'
import projectCascader from '@comp-base/project-cascader'
import { DP } from '@/settings/config'
import { businessTypeEnum } from '@enum-ms/contract'
import { invoiceTypeEnum } from '@enum-ms/finance'
import { contractCollectionInfo } from '@/api/contract/collection-and-invoice/collection'
import useTableValidate from '@compos/form/use-table-validate'
import userDeptCascader from '@comp-base/user-dept-cascader.vue'
import { digitUppercase } from '@/utils/data-type/number'
import { isNotBlank } from '@data-type/index'
import Expense from './expense'

const formRef = ref()
const applyRef = ref()
const collectionRef = ref()
const detailRef = ref()
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
  remark: undefined
}

const { CRUD, crud, form } = regForm(defaultForm, formRef)
const contractInfo = ref({})
const rules = {
  projectId: [{ required: true, message: '请选择项目', trigger: 'change' }],
  applyAmount: [{ required: true, message: '请输入申请金额', trigger: 'blur' }],
  collectionUserId: [{ required: true, message: '请选择收款人', trigger: 'change' }],
  applyUserId: [{ required: true, message: '请选择申请人', trigger: 'change' }],
  applyDate: [{ required: true, message: '请选择申请日期', trigger: 'change' }]
}

const tableRules = {
  choseId: [{ required: true, message: '请选择报销种类', trigger: 'change' }],
  applyAmount: [{ required: true, message: '请输入申请金额', trigger: 'change', type: 'number' }]
}
const { tableValidate, wrongCellMask } = useTableValidate({ rules: tableRules })

watch(
  () => form.projectId,
  (val) => {
    if (val) {
      getContractInfo(val)
    } else {
      contractInfo.value = {}
    }
  },
  { deep: true, immediate: true }
)

function getCollectionChange() {
  const val = collectionRef.value.getNodeInfo()
  if (isNotBlank(val)) {
    form.collectionUserName = val.label
  } else {
    form.collectionUserName = undefined
  }
}

function getDept() {
  if (form.applyUserId) {
    const val = applyRef.value.getNodeInfo()
    form.applyDepartId = val.parentDeptId
    form.applyDepartName = val.parentDeptName
    form.applyUserName = val.label
    if (!form.collectionUserId) {
      form.collectionUserId = form.applyUserId
    }
  } else {
    form.applyDepartId = undefined
    form.applyDepartName = undefined
    form.applyUserName = undefined
  }
}

function applyAmountChange(amount, index) {
  if (amount) {
    form.detailList[index].invoiceAmount = amount
  }
  if (form.detailList.length > 0) {
    let val = 0
    form.detailList.forEach((v) => {
      if (v.applyAmount > 0) {
        val += v.applyAmount
      }
    })
    form.applyAmount = val
  } else {
    form.applyAmount = undefined
  }
}
const upperYuan = computed(() => {
  return form.applyAmount ? digitUppercase(form.applyAmount) : ''
})

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

function expenseChange(val) {
  if (val) {
    if (val.type === 1) {
      form.detailList[val.dataIndex].expenseTypeId = val.id
      form.detailList[val.dataIndex].dictionaryId = undefined
    } else {
      form.detailList[val.dataIndex].expenseTypeId = val.parentId
      form.detailList[val.dataIndex].dictionaryId = val.id
    }
  }
}

function invoiceTypeChange(index) {
  if (form.detailList[index].taxRate && form.detailList[index].invoiceAmount) {
    form.detailList[index].inputTax = ((form.detailList[index].invoiceAmount * form.detailList[index].taxRate) / 100).toFixed(DP.YUAN)
  } else {
    form.detailList[index].inputTax = undefined
  }
}

function deleteRow(index) {
  form.detailList.splice(index, 1)
  applyAmountChange()
}

function addRow() {
  crud.form.detailList.push({
    applyAmount: undefined,
    choseId: undefined,
    dictionaryId: undefined,
    expenseTypeId: undefined,
    inputTax: undefined,
    invoiceAmount: undefined,
    invoiceNo: undefined,
    invoiceType: undefined,
    taxRate: undefined,
    verify: {}
  })
}

CRUD.HOOK.beforeValidateCU = (crud, form) => {
  const { validResult, dealList } = tableValidate(crud.form.detailList)
  if (validResult) {
    crud.form.detailList = dealList
  } else {
    return validResult
  }
}
</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
.table-form {
  ::v-deep(.el-input__inner) {
    padding: 0;
    padding-left: 5px;
  }
}
.add-row-box {
  text-align: center;
  margin-top: 20px;
}
</style>
