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
            <el-form-item label="申请人">
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
              <span>{{ toThousand(detailInfo.paymentAmount) }}</span>
            </el-form-item>
          </el-col>
          <el-col :span="11">
            <el-form-item label="本次付款">
              <!-- <span>{{ detailInfo.serialNumber }}</span> -->
            </el-form-item>
          </el-col>
        </el-row>
        <span style="color:red;font-size:12px;">*请勾选本次付款明细</span>
        <common-table
          ref="detailRef"
          border
          :data="detailInfo.freightDetails"
          :max-height="maxHeight"
          style="width: 100%;"
          class="table-form"
          return-source-data
          @selection-change="tableChange"
          :showEmptySymbol="false"
        >
          <el-table-column type="selection" width="55" align="center"/>
          <el-table-column key="propertyType" prop="propertyType" label="承运属性" align="center" >
            <template v-slot="scope">
              <div>{{ scope.row.propertyType? supplierPayTypeEnum.VL[scope.row.propertyType]: '-' }}</div>
            </template>
          </el-table-column>
          <el-table-column key="freight" prop="freight" label="运费总额" align="center" min-width="120" :show-overflow-tooltip="true">
            <template v-slot="scope">
              <div>{{ toThousand(scope.row.freight) }}</div>
            </template>
          </el-table-column>
          <el-table-column key="paymentAmount" prop="paymentAmount" label="已付款" align="center" min-width="120" :show-overflow-tooltip="true">
            <template v-slot="scope">
              <div>{{ toThousand(scope.row.paymentAmount) }}</div>
            </template>
          </el-table-column>
           <el-table-column key="unPaymentAmount" prop="unPaymentAmount" label="未付款" align="center" min-width="120" :show-overflow-tooltip="true">
            <template v-slot="scope">
              <div>{{ toThousand(scope.row.freight-scope.row.paymentAmount) }}</div>
            </template>
          </el-table-column>
        </common-table>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, defineProps } from 'vue'
import { regForm } from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import { supplierPayTypeEnum } from '@enum-ms/contract'
import { toThousand } from '@data-type/number'
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
