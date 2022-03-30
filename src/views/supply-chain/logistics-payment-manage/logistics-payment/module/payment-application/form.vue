<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    :wrapper-closable="false"
    size="50%"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">提交审核</common-button>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="150px">
        <el-row :gutter="40">
          <el-col :span="11">
            <el-form-item label="申请部门">
              <span>{{ detailInfo.serialNumber }}</span>
            </el-form-item>
          </el-col>
          <el-col :span="11">
            <el-form-item label="付款日期" prop="paymentDate">
              <el-date-picker
                v-model="form.paymentDate"
                type="date"
                value-format="x"
                placeholder="选择付款日期"
                style="width: 160px"
                :disabledDate="(date) => {return date.getTime() < new Date().getTime() - 1 * 24 * 60 * 60 * 1000}"
              />
            </el-form-item>
          </el-col>
        </el-row>
        <el-row :gutter="40">
          <el-col :span="11">
            <el-form-item label="累计已付">
              <span>{{ detailInfo.serialNumber }}</span>
            </el-form-item>
          </el-col>
          <el-col :span="11">
            <el-form-item label="本次付款">
              <span>{{ detailInfo.serialNumber }}</span>
            </el-form-item>
          </el-col>
        </el-row>
        <span style="color:red;font-size:12px;">*请勾选本次付款明细</span>
        <common-table
          ref="detailRef"
          border
          :data="detailInfo.list"
          :max-height="maxHeight"
          style="width: 100%;"
          class="table-form"
          return-source-data
          @selection-change="tableChange"
          :showEmptySymbol="false"
        >
          <el-table-column type="selection" width="55" align="center"/>
          <el-table-column key="branchCompanyId" prop="branchCompanyId" label="承运属性" align="center" min-width="120" :show-overflow-tooltip="true">
            <template v-slot="scope">
              <div>{{ scope.row.branchCompanyName }}</div>
            </template>
          </el-table-column>
          <el-table-column key="supplierId" prop="supplierId" label="项目/采购单号" align="center" min-width="120" :show-overflow-tooltip="true">
            <template v-slot="scope">
              <div>{{ scope.row.supplierName }}</div>
            </template>
          </el-table-column>
          <el-table-column key="supplierId" prop="supplierId" label="运费总额" align="center" min-width="120" :show-overflow-tooltip="true">
            <template v-slot="scope">
              <div>{{ scope.row.supplierName }}</div>
            </template>
          </el-table-column>
          <el-table-column key="supplierId" prop="supplierId" label="已付款" align="center" min-width="120" :show-overflow-tooltip="true">
            <template v-slot="scope">
              <div>{{ scope.row.supplierName }}</div>
            </template>
          </el-table-column>
           <el-table-column key="supplierId" prop="supplierId" label="未付款" align="center" min-width="120" :show-overflow-tooltip="true">
            <template v-slot="scope">
              <div>{{ scope.row.supplierName }}</div>
            </template>
          </el-table-column>
        </common-table>
        <!-- <el-form-item label="采购单号">
          <span>{{ detailInfo.serialNumber }}</span>
        </el-form-item>
        <el-form-item label="所属项目">
          <template v-if="detailInfo.projects && detailInfo.projects.length>0">
            <div v-for="item in detailInfo.projects" :key="item.id">
              {{item.serialNumber+' '+item.shortName}}
            </div>
          </template>
        </el-form-item>
        <el-form-item label="供应商">
          <span>{{ detailInfo.supplierName }}</span>
        </el-form-item>
        <el-form-item label="合同额(元)">
          <span>{{ detailInfo.amount?toThousand(detailInfo.amount):'-' }}</span>
        </el-form-item>
        <el-form-item label="已付款(元)">
          <span>{{ detailInfo.paymentAmount?toThousand(detailInfo.paymentAmount):'-' }}</span>
          <el-tag style="margin-left:5px;" v-if="detailInfo.amount">{{ (detailInfo.paymentAmount/detailInfo.amount)*100+'%' }}</el-tag>
        </el-form-item>
        <el-form-item label="最后一次付款(元)">
          <span>{{ detailInfo.lastPaymentAmount?toThousand(detailInfo.lastPaymentAmount):'-' }}</span>
          <span v-if="detailInfo.lastPaymentTime" style="margin-left:5px;">{{ parseTime(detailInfo.lastPaymentTime,'{y}-{m}-{d}')}}</span>
        </el-form-item>
        <el-form-item label="付款日期" prop="paymentDate">
          <el-date-picker
            v-model="form.paymentDate"
            type="date"
            value-format="x"
            placeholder="选择付款日期"
            style="width: 220px"
            :disabledDate="(date) => {return date.getTime() < new Date().getTime() - 1 * 24 * 60 * 60 * 1000}"
          />
        </el-form-item>
        <el-form-item label="本次付款(元)"  prop="applyAmount">
          <el-input-number
              v-show-thousand
              v-model="form.applyAmount"
              :step="1"
              :min="0"
              :max="detailInfo.amount?detailInfo.amount-detailInfo.paymentAmount:999999999999"
              :precision="DP.YUAN"
              controls-position="right"
              style="width: 220px"
            />
            <span style="color:#82848a">{{form.applyAmount?digitUppercase(form.applyAmount):''}}</span>
        </el-form-item>
        <el-form-item label="付款事由" prop="paymentReasonId">
          <common-select
            v-model="form.paymentReasonId"
            :options="dict.payment_reason"
            type="dict"
            size="small"
            clearable
            placeholder="付款事由"
            style="width: 220px"
          />
        </el-form-item>
        <el-form-item label="收款银行">
          <span>{{detailInfo.supplierBankName}}</span>
        </el-form-item>
        <el-form-item label="账号">
          <span>{{detailInfo.supplierBankAccount}}</span>
        </el-form-item>
        <el-form-item label="附件">
          <upload-btn ref="uploadRef" v-model:files="form.attachments" :file-classify="fileClassifyEnum.CONTRACT_ATT.V" :limit="1" :accept="'.zip,.jpg,.png,.pdf,.jpeg'"/>
        </el-form-item> -->
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, defineProps } from 'vue'
import { regForm } from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
// import { digitUppercase, toThousand } from '@data-type/number'
// import { parseTime } from '@/utils/date'
// import useDict from '@compos/store/use-dict'
// import { fileClassifyEnum } from '@enum-ms/file'
// import { DP } from '@/settings/config'
// import UploadBtn from '@comp/file-upload/UploadBtn'

const formRef = ref()
const props = defineProps({
  detailInfo: {
    type: Object,
    default: () => {}
  }
})
const defaultForm = {
  paymentDate: undefined,
  applyAmount: undefined,
  attachmentIds: undefined,
  attachments: undefined,
  orderId: undefined,
  paymentReasonId: undefined,
  propertyType: undefined,
  receiveBank: undefined,
  receiveBankAccount: undefined
}

const { maxHeight } = useMaxHeight({
  wrapperBox: '.paymentAddForm',
  paginate: true,
  extraHeight: 40
})

const { CRUD, crud, form } = regForm(defaultForm, formRef)

const validateMoney = (rule, value, callback) => {
  if (!value) {
    callback(new Error('请填写申请金额并大于0'))
  }
  callback()
}

const rules = {
  paymentDate: [{ required: true, message: '请选择付款日期', trigger: 'change' }],
  paymentReasonId: [{ required: true, message: '请选择付款事由', trigger: 'change' }],
  applyAmount: [{ required: true, validator: validateMoney, trigger: 'blur' }]
}

function tableChange() {

}

CRUD.HOOK.beforeSubmit = () => {
  crud.form.orderId = props.detailInfo.id
  // crud.form.propertyType = props.detailInfo.propertyType
  // crud.form.receiveBank = props.detailInfo.supplierBankName || undefined
  // crud.form.receiveBankAccount = props.detailInfo.supplierBankAccount || undefined
}
</script>
<style lang="scss" scoped>
.add-row-box {
  text-align: center;
  margin-top: 20px;
}
</style>
