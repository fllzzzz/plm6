<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelDetail"
    :visible="crud.detailVisible"
    :title="detailTitle"
    :show-close="true"
    size="50%"
  >
    <template #titleRight>
      <export-button v-permission="crud.permission.download" :params="detail.id" :fn="download"> 下载结算单 </export-button>
      <span v-if="props.status === reviewStatusEnum.UNREVIEWED.V">
        <common-button :loading="submitLoading" size="mini" type="success" @click="handleSubmit(true)">确 签</common-button>
        <el-popconfirm title="确定要拒绝此条结算单吗？" @confirm="handleSubmit(false)">
          <template #reference>
            <common-button :loading="submitLoading" size="mini" type="danger">拒 绝</common-button>
          </template>
        </el-popconfirm>
      </span>
    </template>
    <template #content>
      <el-form v-loading="crud.detailLoading" ref="formRef" size="mini" label-width="100px" class="demo-form">
        <div class="rule-row">
          <el-form-item label="项目" prop="project">
            <span class="project-name">{{ projectNameFormatter(detail.project) }}</span>
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="合同内容" prop="projectContentName">
            <div>{{ detail?.project?.projectContentName }}</div>
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="结算日期" prop="settlementDate">
            <div v-parse-time="{ val: detail.settlementDate, fmt: '{y}-{m}-{d}' }" />
          </el-form-item>
          <el-form-item label="申请人" prop="userName">
            <div>{{ detail.userName }}</div>
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="签约单位" prop="contractSignBodyName">
            <div>{{ detail?.project?.contractSignBodyName }}</div>
          </el-form-item>
          <el-form-item label="发包单位" prop="customerUnit">
            <div>{{ detail?.project?.customerUnit }}</div>
          </el-form-item>
        </div>
         <div class="rule-row">
          <el-form-item label="签约人" prop="signerName">
            <div>{{ detail?.project?.signerName }}</div>
          </el-form-item>
          <el-form-item label="合同含税" prop="isTax">
            <div>
              <span>{{ isTaxContractEnum.V?.[detail?.project?.isTax]?.['SL'] }}</span>
              <span v-if="detail?.project?.isTax === isTaxContractEnum.YES.V">【{{ invoiceTypeEnum.VL?.[detail?.project?.invoiceType] }} {{detail?.project?.taxRate || 0 }}%】</span>
              </div>
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="签订日期" prop="signingDate">
            <div v-parse-time="{ val: detail?.project?.signingDate, fmt: '{y}-{m}-{d}' }" />
          </el-form-item>
          <el-form-item label="合同额" prop="contractAmount">
            <div><span v-thousand="{val:detail?.project?.contractAmount ||0, dp:decimalPrecision.contract}" />（{{ digitUppercase(detail?.project?.contractAmount || 0) }}）</div>
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="保证金额" prop="marginAmount">
            <div v-if="isBlank(detail?.project?.marginAmount)">无</div>
            <div v-else>
              <div><span v-thousand="{val:detail?.project?.marginAmount ||0, dp:decimalPrecision.contract}" />（{{ digitUppercase(detail?.project?.marginAmount || 0) }}）</div>
              <span v-if="dict.label['margin_type'] && detail?.project?.marginType">（{{ dict.label['margin_type'][detail?.project?.marginType] }}）</span>
            </div>
          </el-form-item>
          <el-form-item label="累计发运额" prop="happenedAmount">
            <div v-if="isBlank(detail?.project?.happenedAmount)">无</div>
            <div v-else><span v-thousand="{val:detail?.project?.happenedAmount ||0, dp:decimalPrecision.contract}" />（{{ digitUppercase(detail?.project?.happenedAmount || 0) }}）</div>
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="违约金额" prop="breachAmount">
            <div v-if="isBlank(detail.breachAmount)">无</div>
            <div v-else>{{toThousand(detail.breachAmount,decimalPrecision.contract)}}（{{ digitUppercase(detail.breachAmount) }}）</div>
          </el-form-item>
          <el-form-item label="签证额" prop="visaAmount">
            <div v-if="isBlank(detail.visaAmount)">无</div>
            <div v-else>{{toThousand(detail.visaAmount,decimalPrecision.contract)}}（{{ digitUppercase(detail.visaAmount) }}）</div>
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="加工结算额" prop="processingSettlementAmount">
            <div><span v-thousand="{val:detail.processingSettlementAmount ||0, dp:decimalPrecision.contract}" />（{{ digitUppercase(detail.processingSettlementAmount) }}）</div>
          </el-form-item>
          <el-form-item label="最终结算额" prop="settlementAmount">
            <div><span v-thousand="{val:detail.settlementAmount ||0, dp:decimalPrecision.contract}" />（{{ digitUppercase(detail.settlementAmount) }}）</div>
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="累计收款" prop="collectionAmount">
            <div>
              <span v-thousand="{val:detail?.project?.collectionAmount ||0, dp:decimalPrecision.contract}" />
              （{{ digitUppercase(detail?.project?.collectionAmount || 0) }}）
            </div>
          </el-form-item>
          <el-form-item label="结算应收" prop="debitAmount">
            <div><span v-thousand="{val:debitAmount ||0, dp:decimalPrecision.contract}" />（{{ digitUppercase(debitAmount || 0) }}）</div>
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="累计开票" prop="invoiceAmount">
            <div><span v-thousand="{val:detail?.project?.invoiceAmount ||0, dp:decimalPrecision.contract}" />（{{ digitUppercase(detail?.project?.invoiceAmount || 0) }}）</div>
          </el-form-item>
          <el-form-item label="应补发票" prop="debitInvoice">
            <div v-if="detail?.project?.isTax === isTaxContractEnum.YES.V"><span v-thousand="{val:debitInvoice ||0, dp:decimalPrecision.contract}" />（{{ digitUppercase(debitInvoice || 0) }}）</div>
            <div v-else>无</div>
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="结算备注" prop="remark">
            <div>{{ detail.remark }}</div>
          </el-form-item>
        </div>
        <div class="rule-row" v-if="props.status === reviewStatusEnum.PASS.V">
          <el-form-item label="确签备注" prop="attachmentRemark">
            <div>{{ detail.attachmentRemark }}</div>
          </el-form-item>
        </div>
        <div class="item-center">
          <upload-list
            :uploadable="props.status === reviewStatusEnum.UNREVIEWED.V"
            :file-classify="fileClassifyEnum.CONTRACT_VISA.V"
            :accept="'.jpg,.png,.pdf,.jpeg'"
            showView
            showDownload
            v-model:files="detail.files"
            style="padding-bottom: 20px"
          >
          <template #applicant>
              <el-table-column label="上传人" align="center" min-width="60" :show-overflow-tooltip="true" prop="createUserName">
              </el-table-column>
            </template>
        </upload-list>
          <el-input
            v-if="props.status === reviewStatusEnum.UNREVIEWED.V"
            v-model="detail.attachmentRemark"
            type="textarea"
            :autosize="{ minRows: 2, maxRows: 4 }"
            placeholder="请填写确签备注"
            maxlength="200"
            show-word-limit
          />
        </div>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { check, download } from '@/api/contract/sales-manage/settlement-manage'
import { ref, defineProps, defineEmits, computed } from 'vue'

import { isBlank } from '@data-type/index'
import { fileClassifyEnum } from '@enum-ms/file'
import { isTaxContractEnum } from '@enum-ms/contract'
import { invoiceTypeEnum } from '@/utils/enum/modules/finance'
import { projectNameFormatter } from '@/utils/project'
import { reviewStatusEnum } from '@enum-ms/common'
import { digitUppercase } from '@data-type/number'
import { toThousand } from '@/utils/data-type/number'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

import { regDetail } from '@compos/use-crud'
import useDict from '@compos/store/use-dict'
import uploadList from '@comp/file-upload/UploadList.vue'
import ExportButton from '@comp-common/export-button/index.vue'

const { decimalPrecision } = useDecimalPrecision()

const emit = defineEmits(['success'])

const props = defineProps({
  status: {
    // 签证单状态
    type: Number,
    default: undefined
  }
})

// 标题
const detailTitle = computed(() => {
  return props.status === reviewStatusEnum.UNREVIEWED.V ? '结算单待确签' : '结算单详情'
})

// 欠款额
const debitAmount = computed(() => {
  return (detail.settlementAmount || 0) - (detail?.project?.collectionAmount || 0)
})

// 应补发票
const debitInvoice = computed(() => {
  return (detail.settlementAmount || 0) - (detail?.project?.invoiceAmount || 0)
})

const formRef = ref()
const submitLoading = ref(false)

const dict = useDict(['margin_type'])
const { crud, detail, CRUD } = regDetail()

// 详情加载后
CRUD.HOOK.beforeDetailLoaded = async (crud) => {
  try {
    detail.files = detail.attachments || []
  } catch (error) {
    crud.notify('获取结算单详情失败', CRUD.NOTIFICATION_TYPE.ERROR)
  }
}

// 结算单确签
async function handleSubmit(status) {
  submitLoading.value = true
  try {
    const params = {
      id: detail.id
    }
    if (status) {
      if (!detail.files.length) {
        crud.notify('请上传确签附件', CRUD.NOTIFICATION_TYPE.WARNING)
        return
      }
      params.status = reviewStatusEnum.PASS.V
      params.attachmentRemark = detail.attachmentRemark
      params.attachmentIds = detail.files.map((f) => f.id)
    } else {
      params.status = reviewStatusEnum.REFUSE.V
    }
    await check(params)
    crud.cancelDetail()
    emit('success')
  } catch (error) {
    crud.notify('提交结算单确签信息', CRUD.NOTIFICATION_TYPE.ERROR)
  } finally {
    submitLoading.value = false
  }
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
.form {
  padding: 0px 10px 10px;
}
.demo-form .rule-row {
  display: flex;
  margin-bottom: 10px;
}
.demo-form .rule-row:last-child {
  margin-bottom: 0px;
}
.demo-form .el-form-item {
  flex: 1;
  width: 50%;
  margin-right: 10px;
}
.demo-form .el-upload__tip {
  padding-left: 15px;
}
.item-center {
  margin: 10px;
}
</style>
