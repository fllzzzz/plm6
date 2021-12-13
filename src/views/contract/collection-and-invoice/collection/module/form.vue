<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    :wrapper-closable="false"
    size="860px"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="140px">
      <div class="form-row" style="display:flex;">
        <el-form-item label="项目" prop="projectId">
          <project-cascader
            v-model="form.projectId"
            style="width:250px"
            class="filter-item"
          />
        </el-form-item>
        <el-form-item label="收款日期" prop="collectionDate">
          <el-date-picker
            v-model="form.collectionDate"
            type="date"
            value-format="x"
            placeholder="选择收款日期"
            style="width: 250px;"
          />
        </el-form-item>
      </div>
      <div class="form-row" style="display:flex;">
        <el-form-item label="合同金额(元)" prop="contractAmount">
          <el-input
            v-model="contractInfo.contractAmount"
            type="text"
            placeholder="合同金额"
            style="width: 250px;"
            disabled
          />
        </el-form-item>
        <el-form-item label="收款单位" prop="collectionUnitId">
          <common-select
            v-model="form.collectionUnitId"
            :options="contractInfo.companyBankAccountList"
            :type="'other'"
            :dataStructure="typeProp"
            size="small"
            clearable
            class="filter-item"
            placeholder="收款单位"
            style="width:250px"
            @change="collectionCompanyChange"
          />
        </el-form-item>
      </div>
      <div class="form-row" style="display:flex;">
        <el-form-item label="已收款额(元)">
          <el-input
            v-model="contractInfo.collectionSumAmount"
            type="text"
            placeholder="已收款额"
            style="width: 250px;"
            disabled
          />
        </el-form-item>
        <el-form-item label="收款行" prop="collectionDepositBank">
          <el-input
            v-model="form.collectionDepositBank"
            type="text"
            placeholder="收款行"
            style="width: 250px;"
          />
        </el-form-item>
      </div>
      <div class="form-row" style="display:flex;">
        <el-form-item label="本次收款金额(元)" prop="collectionAmount">
          <el-input-number
            v-model.number="form.collectionAmount"
            :min="-99999999999"
            :max="99999999999"
            :step="10000"
            :precision="DP.YUAN"
            placeholder="本次收款金额(元)"
            controls-position="right"
            style="width: 250px;"
          />
        </el-form-item>
        <el-form-item label="收款账号" prop="collectionBankAccount">
          <el-input
            v-model="form.collectionBankAccount"
            type="text"
            placeholder="收款账号"
            style="width: 250px;"
          />
        </el-form-item>
      </div>
      <div class="form-row" style="display:flex;">
        <el-form-item label="收款金额大写" prop="paymentAmount1">
          <el-input
            v-model="upperYuan"
            placeholder="收款金额大写"
            style="width: 250px;"
            disabled
          />
        </el-form-item>
        <el-form-item label="付款单位" prop="paymentUnit">
          <el-input
            v-model="form.paymentUnit"
            type="text"
            placeholder="付款单位"
            style="width: 250px;"
          />
        </el-form-item>
      </div>
      <div class="form-row" style="display:flex;">
        <el-form-item label="收款事由" prop="collectionReason">
          <common-select
            v-model="form.collectionReason"
            :options="dict.payment_reason"
            type="dict"
            size="small"
            clearable
            placeholder="收款事由"
            style="width:250px"
          />
        </el-form-item>
        <el-form-item label="付款行" prop="paymentDepositBank">
          <el-input
            v-model="form.paymentDepositBank"
            type="text"
            placeholder="付款行"
            style="width: 250px;"
          />
        </el-form-item>
      </div>
      <div class="form-row" style="display:flex;">
        <el-form-item label="收款方式" prop="collectionMode">
          <template #label>
            收款方式
            <el-tooltip
              effect="light"
              :content="`选择承兑汇票可在页面下方添加承兑汇票信息`"
              placement="top"
            >
              <i class="el-icon-info" />
            </el-tooltip>
          </template>
          <common-select
            v-model="form.collectionMode"
            :options="paymentFineModeEnum.ENUM"
            type="enum"
            size="small"
            placeholder="收款方式"
            style="width: 250px;"
          />
        </el-form-item>
        <el-form-item label="付款账号" prop="paymentBankAccount">
          <el-input
            v-model="form.paymentBankAccount"
            type="text"
            placeholder="付款账号"
            style="width: 250px;"
          />
        </el-form-item>
      </div>
      <el-collapse-transition>
        <div v-show="form.collectionMode === paymentFineModeEnum.ENUM.ACCEPTANCE_DRAFT.V" class="table-box">
          <common-table
            ref="table"
            :data="form.acceptanceDrafts"
            :cell-class-name="handelCellClassName"
            style="width: 100%"
          >
            <el-table-column label="序号" type="index" align="center" width="60" />
            <el-table-column prop="amount" label="承兑面额（元）" align="center" min-width="160">
              <template v-slot="scope">
                <el-input-number
                  v-model="scope.row.amount"
                  :min="1"
                  :max="99999999999"
                  :step="10000"
                  :precision="DP.YUAN"
                  size="small"
                  controls-position="right"
                  placeholder="承兑面额（元）"
                  style="width:100%;max-width:200px;"
                />
              </template>
            </el-table-column>
            <el-table-column prop="discount" label="贴现利息（元）" align="center" min-width="160">
              <template v-slot="scope">
                <el-input-number
                  v-model="scope.row.discount"
                  :max="scope.row.amount || 0"
                  :step="10000"
                  :precision="DP.YUAN"
                  controls-position="right"
                  placeholder="贴现利息（元）"
                  size="small"
                  style="width:100%;max-width:200px;"
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
            <common-button size="mini" icon="el-icon-circle-plus-outline" type="warning" style="margin-right:15px" @click="addRow()">继续添加</common-button>
            <el-tooltip
              effect="light"
              :content="`1.贴现利息不可超过承兑面额\n
              2.承兑汇票总金额不可超过付款金额\n`"
              placement="right"
            >
              <div style="display:inline-block;">
                <el-tag type="info">承兑汇票添加规则</el-tag>
              </div>
            </el-tooltip>
          </div>
          <hr class="gradient-line">
        </div>
      </el-collapse-transition>
      <el-form-item label="备注" prop="remark">
        <el-input
          v-model="form.remark"
          type="textarea"
          :autosize="{ minRows: 6, maxRows: 8}"
          placeholder="可填写备注"
          style="max-width: 500px;"
        />
      </el-form-item>
    </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, watch, computed } from 'vue'
import { regForm } from '@compos/use-crud'
import projectCascader from '@comp-base/project-cascader'
import useDict from '@compos/store/use-dict'
import { DP } from '@/settings/config'
import { paymentFineModeEnum } from '@enum-ms/finance'
import { contractCollectionInfo } from '@/api/contract/collection-and-invoice/collection'
import { digitUppercase } from '@/utils/data-type/number'

const formRef = ref()
const dict = useDict(['payment_reason'])
const typeProp = { key: 'companyId', label: 'companyName', value: 'companyId' }
const defaultForm = {
  id: undefined,
  collectionAmount: undefined,
  collectionBankAccount: undefined,
  collectionDate: undefined,
  collectionDepositBank: undefined,
  collectionReason: undefined,
  collectionUnit: undefined,
  collectionUnitId: undefined,
  paymentBankAccount: undefined,
  paymentDepositBank: undefined,
  paymentUnit: undefined,
  projectId: undefined,
  remark: undefined,
}

const { crud, form } = regForm(defaultForm, formRef)

const contractInfo = ref({})

const rules = {
  projectId: [{ required: true, message: '请选择项目', trigger: 'change' }],
  collectionAmount: [{ required: true, message: '请输入本次收款金额', trigger: 'change', type: 'number' }],
  collectionReason: [{ required: true, message: '请选择收款事由', trigger: 'change' }],
  collectionMode: [{ required: true, message: '请选择收款方式', trigger: 'change' }],
  collectionDate: [{ required: true, message: '请选择收款日期', trigger: 'change' }],
  collectionUnitId: [{ required: true, message: '请选择收款单位', trigger: 'change' }],
  paymentUnit: [{ required: true, message: '请输入付款单位', trigger: 'blur' }]
}
const upperYuan = computed(()=>{
  return form.collectionAmount? digitUppercase(form.collectionAmount): ''
})
watch(
  () => form.projectId,
  (val) => {
    if (val) {
      getContractInfo(val)
    }else{
      contractInfo.value = {}
    }
  },
  { deep: true, immediate: true }
)

async function getContractInfo(id){
  let data = {}
  try{
    data = await contractCollectionInfo({projectId:id})
  }catch(e){
    console.log('获取合同信息',e)
  }finally{
    contractInfo.value = data
    form.paymentBankAccount = contractInfo.value.customerBankCode
    form.paymentDepositBank = contractInfo.value.customerBankName
    form.paymentUnit = contractInfo.value.customerUnit
  }
}

function collectionCompanyChange(val){
  if(val){
    const collectionVal = contractInfo.value.companyBankAccountList.find(v=>v.companyId===val)
    form.collectionBankAccount = collectionVal.account
    form.collectionDepositBank = collectionVal.depositBank
    form.collectionUnit = collectionVal.companyName
  } else {
    form.collectionBankAccount = ''
    form.collectionDepositBank = ''
    form.collectionUnit = ''
  }
}

function handelCellClassName(){

}
</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>
