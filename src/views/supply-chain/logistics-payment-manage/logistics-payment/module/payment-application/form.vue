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
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="60px" label-position="left">
        <common-table
          ref="detailRef"
          border
          :data="form.paymentDetails"
          :max-height="maxHeight"
          style="width: 100%;margin-bottom:10px;"
          class="table-form"
          return-source-data
          :showEmptySymbol="false"
          v-loading="tableLoading"
          show-summary
          :summary-method="getSummaries"
        >
          <el-table-column key="projectName" prop="projectName" label="项目/采购合同编号" align="center" >
            <template v-slot="scope">
              <div>{{ scope.row.projectName || scope.row.serialNumber  }}</div>
            </template>
          </el-table-column>
          <el-table-column key="type" prop="type" label="运输属性" align="center" >
            <template v-slot="scope">
              <div>{{ scope.row.type? logisticsSearchTypeEnum.VL[scope.row.type]: '-' }}</div>
            </template>
          </el-table-column>
          <el-table-column key="freight" prop="freight" label="运费总额" align="center" min-width="120" :show-overflow-tooltip="true">
            <template v-slot="scope">
              <div>{{ toThousand(scope.row.freight) }}</div>
            </template>
          </el-table-column>
          <el-table-column key="paymentAmount" prop="paymentAmount" label="已支付" align="center" min-width="120" :show-overflow-tooltip="true">
            <template v-slot="scope">
              <div>{{ toThousand(scope.row.paymentAmount) }}</div>
            </template>
          </el-table-column>
           <el-table-column key="applyAmount" prop="applyAmount" label="本次支付金额(元)" align="center" min-width="120" :show-overflow-tooltip="true">
            <template v-slot="scope">
              <el-input-number
                v-model.number="scope.row.applyAmount"
                v-show-thousand
                :min="scope.row.paymentAmount>scope.row.freight?(-(scope.row.paymentAmount-scope.row.freight)):0"
                :max="scope.row.paymentAmount>scope.row.freight?0:scope.row.freight-scope.row.paymentAmount"
                :step="100"
                :precision="DP.YUAN"
                placeholder="本次支付(元)"
                controls-position="right"
              />
            </template>
          </el-table-column>
        </common-table>
        <el-form-item label="附件">
          <template #label>
            附件
            <el-tooltip
              effect="light"
              :content="`双击可预览附件`"
              placement="top"
              v-if="form.id && form.attachments?.length && !form.files?.length"
            >
              <i class="el-icon-info" />
            </el-tooltip>
          </template>
          <template v-if="form.id && form.attachments?.length && !form.files?.length">
            <div v-for="item in form.attachments" :key="item.id">
              <div style="cursor:pointer;" @dblclick="attachmentView(item)">{{item.name}}</div>
            </div>
          </template>
          <upload-btn ref="uploadRef" v-model:files="form.files" :file-classify="fileClassifyEnum.CONTRACT_ATT.V" :limit="1" :accept="'.jpg,.png,.pdf,.jpeg'"/>
        </el-form-item>
      </el-form>
      <showPdfAndImg v-if="pdfShow" :isVisible="pdfShow" :showType="'attachment'" :id="currentId" @close="pdfShow=false"/>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, defineProps } from 'vue'
import { regForm } from '@compos/use-crud'
import { tableSummary } from '@/utils/el-extra'
import useMaxHeight from '@compos/use-max-height'
import { logisticsSearchTypeEnum } from '@enum-ms/contract'
import { toThousand } from '@data-type/number'
import { DP } from '@/settings/config'
import { fileClassifyEnum } from '@enum-ms/file'
import UploadBtn from '@comp/file-upload/UploadBtn'
import { ElMessage } from 'element-plus'
import showPdfAndImg from '@comp-base/show-pdf-and-img.vue'

const formRef = ref()
const props = defineProps({
  detailInfo: {
    type: Object,
    default: () => {}
  }
})
const defaultForm = {
  applyAmount: undefined,
  attachmentIds: undefined,
  files: undefined,
  branchCompanyId: undefined,
  paymentReasonId: 1,
  supplierId: undefined,
  detailSaveParams: []
}

const { maxHeight } = useMaxHeight({
  wrapperBox: '.paymentAddForm',
  paginate: true,
  extraHeight: 120
})

const { CRUD, crud, form } = regForm(defaultForm, formRef)
const pdfShow = ref(false)
const currentId = ref()

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

const tableLoading = ref(false)

// 预览附件
function attachmentView(item) {
  currentId.value = item.id
  pdfShow.value = true
}

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: [['freight', DP.YUAN], ['paymentAmount', DP.YUAN], ['applyAmount', DP.YUAN]],
    toThousandFields: ['freight', 'paymentAmount', 'applyAmount']
  })
}

CRUD.HOOK.beforeSubmit = () => {
  crud.form.attachmentIds = crud.form.files ? crud.form.files.map((v) => v.id) : (crud.form.attachments ? crud.form.attachments.map((v) => v.id) : undefined)
  crud.form.supplierId = props.detailInfo.supplierId
  crud.form.branchCompanyId = props.detailInfo.branchCompanyId
  crud.form.applyAmount = 0
  const listData = JSON.parse(JSON.stringify(crud.form.paymentDetails))
  const submitData = []
  listData.map(v => {
    if (v.applyAmount !== 0) {
      crud.form.applyAmount += v.applyAmount
      submitData.push(v)
    }
  })
  if (submitData.length === 0) {
    ElMessage.error('请填写本次申请明细')
    return false
  }
  crud.form.paymentDetails = submitData
}
</script>
<style lang="scss" scoped>
.add-row-box {
  text-align: center;
  margin-top: 20px;
}
</style>
