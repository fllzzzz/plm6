<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    :wrapper-closable="false"
    size="600px"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">提交审核</common-button>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="120px">
        <el-form-item label="采购单号">
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
        <el-form-item label="合同额">
          <span>{{ detailInfo.amount }}</span>
        </el-form-item>
        <el-form-item label="已付款">
          <span>{{ detailInfo.paymentAmount }}</span>
          <el-tag style="margin-left:5px;" v-if="detailInfo.amount">{{(detailInfo.paymentAmount/detailInfo.amount)*100+'%'}}</el-tag>
        </el-form-item>
        <el-form-item label="最后一次付款">
          <span v-if="detailInfo.lastPaymentAmount">{{toThousand(detailInfo.lastPaymentAmount)}}</span>
          <span v-if="detailInfo.lastPaymentTime">{{ parseTime(detailInfo.lastPaymentTime,'{y}-{m}-{d}')}}</span>
        </el-form-item>
        <el-form-item label="本次付款" >
          <el-input-number
              v-show-thousand
              v-model="form.applyAmount"
              :step="1"
              :min="0"
              :max="detailInfo.amount?detailInfo.amount-detailInfo.paymentAmount:999999999999"
              :precision="DP.YUAN"
              controls-position="right"
              style="width: 220px"
              placeholder="金额(元)"
            />
            <div style="color:#82848a">{{form.applyAmount?digitUppercase(form.applyAmount):''}}</div>
        </el-form-item>
        <el-form-item label="收款银行">
          <span>{{detailInfo.supplierBankName}}</span>
        </el-form-item>
        <el-form-item label="账号">
          <span>{{detailInfo.supplierBankAccount}}</span>
        </el-form-item>
        <el-form-item label="付款事由">
          <common-select
            v-model="form.paymentReasonId"
            :options="dict.payment_reason"
            type="dict"
            size="small"
            clearable
            placeholder="付款事由"
            style="width:100%"
          />
        </el-form-item>
        <el-form-item label="附件">
          <upload-btn ref="uploadRef" v-model:files="form.attachments" :file-classify="fileClassifyEnum.CONTRACT_ATT.V" :limit="1" :accept="'.zip,.jpg,.png,.pdf,.jpeg'"/>
        </el-form-item>
        <!-- <el-form-item label="单体名称" prop="name">
          <el-input v-model="form.name" type="text" placeholder="请填写单体名称" style="width: 270px" @blur="form.name=form.name.replace(/[ ]/g,'')"/>
        </el-form-item>
        <el-form-item label="日期" prop="date">
          <el-date-picker
            v-model="form.date"
            type="date"
            value-format="x"
            placeholder="选择完成日期"
            style="width: 160px"
            :disabledDate="(date) => {return date.getTime() < new Date().getTime() - 1 * 24 * 60 * 60 * 1000}"
          />
        </el-form-item>
        <el-form-item label="排序" prop="sort">
          <el-input-number v-model.number="form.sort" :min="1" :max="999" :step="1" controls-position="right" style="width: 270px" />
        </el-form-item>
        <el-form-item label="备注" prop="remark">
          <el-input
            v-model="form.remark"
            type="textarea"
            :autosize="{ minRows: 4, maxRows: 6 }"
            placeholder="请填写备注"
            :maxlength="200"
            show-word-limit
            style="width: 320px"
          />
        </el-form-item> -->
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, defineProps } from 'vue'
import { regForm } from '@compos/use-crud'
import { digitUppercase, toThousand } from '@data-type/number'
import { parseTime } from '@/utils/date'
import useDict from '@compos/store/use-dict'
import { fileClassifyEnum } from '@enum-ms/file'
import { DP } from '@/settings/config'
import UploadBtn from '@comp/file-upload/UploadBtn'

const formRef = ref()
const dict = useDict(['payment_reason'])
const props = defineProps({
  detailInfo: {
    type: Object,
    default: () => {}
  }
})
const defaultForm = {
  applyAmount: undefined,
  attachmentIds: undefined,
  attachments: undefined,
  orderId: undefined,
  paymentReasonId: undefined,
  propertyType: undefined,
  receiveBank: undefined,
  receiveBankAccount: undefined
}
const { CRUD, crud, form } = regForm(defaultForm, formRef)

const validateMoney = (rule, value, callback) => {
  if (!value) {
    callback(new Error('请填写申请金额并大于0'))
  }
  callback()
}

const rules = {
  paymentReasonId: [{ required: true, message: '请选择付款事由', trigger: 'change' }],
  applyAmount: [{ required: true, validator: validateMoney, trigger: 'blur' }]
}

CRUD.HOOK.beforeSubmit = () => {
  crud.form.attachmentIds = crud.form.attachments ? crud.form.attachments.map((v) => v.id) : undefined
  crud.form.orderId = props.detailInfo.id
  crud.form.propertyType = 1
  crud.form.receiveBank = props.detailInfo.supplierBankName || undefined
  crud.form.receiveBankAccount = props.detailInfo.supplierBankAccount || undefined
}
</script>
<style lang="scss" scoped>
.add-row-box {
  text-align: center;
  margin-top: 20px;
}
</style>
