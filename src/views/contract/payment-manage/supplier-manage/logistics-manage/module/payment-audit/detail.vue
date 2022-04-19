<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="handleClose"
    v-model="visible"
    title="审核详情"
    :wrapper-closable="false"
    size="50%"
  >
    <template #titleAfter>
      <el-tag v-if="detailInfo.auditStatus" size="medium" :type="detailInfo.auditStatus===auditTypeEnum.REJECT.V?'info':(detailInfo.auditStatus===auditTypeEnum.PASS.V?'success':'warning')">
        {{ detailInfo.auditStatus===auditTypeEnum.REJECT.V?'已驳回':(detailInfo.auditStatus===auditTypeEnum.PASS.V?'已通过':'审核中') }}
      </el-tag>
    </template>
    <template #titleRight>
      <common-button v-if="showType==='audit'" size="small" type="info" @click="passConfirm(auditTypeEnum.REJECT.V)">驳回</common-button>
      <common-button v-if="showType==='audit'" size="small" type="success" @click="passConfirm(auditTypeEnum.PASS.V)">通过</common-button>
    </template>
    <template #content>
      <el-form ref="formRef" size="small" label-width="60px" label-position="left">
        <common-table
          ref="detailRef"
          border
          :data="detailInfo.paymentDetails"
          :max-height="maxHeight"
          style="width: 100%;margin-bottom:10px;"
          class="table-form"
          return-source-data
          :showEmptySymbol="false"
          v-loading="tableLoading"
          show-summary
          :summary-method="getSummaries"
        >
          <el-table-column key="projectName" prop="projectName" label="项目/采购订单" align="center" >
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
              <div>{{ toThousand(scope.row.applyAmount) }}</div>
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
            >
              <i class="el-icon-info" />
            </el-tooltip>
          </template>
          <template v-if="detailInfo.attachments && detailInfo.attachments.length>0">
            <div v-for="item in detailInfo.attachments" :key="item.id">
              <div style="cursor:pointer;" @dblclick="attachmentView(item)">{{item.name}}</div>
            </div>
          </template>
        </el-form-item>
      </el-form>
      <showPdfAndImg v-if="pdfShow" :isVisible="pdfShow" :showType="'attachment'" :id="currentId" @close="pdfShow=false"/>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, defineProps, defineEmits } from 'vue'
import useVisible from '@compos/use-visible'
import { editStatus } from '@/api/contract/supplier-manage/pay-invoice/pay'
import { tableSummary } from '@/utils/el-extra'
import useMaxHeight from '@compos/use-max-height'
import { logisticsSearchTypeEnum, auditTypeEnum } from '@enum-ms/contract'
import { toThousand } from '@data-type/number'
import { ElNotification } from 'element-plus'
import showPdfAndImg from '@comp-base/show-pdf-and-img.vue'

const formRef = ref()
const props = defineProps({
  detailInfo: {
    type: Object,
    default: () => {}
  },
  modelValue: {
    type: Boolean,
    require: true
  },
  showType: {
    type: String,
    default: undefined
  }
})

const { maxHeight } = useMaxHeight({
  wrapperBox: '.paymentAddForm',
  paginate: true,
  extraHeight: 40
})

const pdfShow = ref(false)
const currentId = ref()
const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

const tableLoading = ref(false)

// 预览附件
function attachmentView(item) {
  currentId.value = item.id
  pdfShow.value = true
}

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: ['freight', 'paymentAmount', 'applyAmount'],
    toThousandFields: ['freight', 'paymentAmount', 'applyAmount']
  })
}

async function passConfirm(val) {
  try {
    const submitData = {
      auditStatus: val,
      actuallyPaymentAmount: props.detailInfo.applyAmount,
      id: props.detailInfo.id
      // paymentBank: paymentBank.value,
      // paymentBankAccount: paymentBankAccount.value,
      // paymentMethod: paymentMethod.value
    }
    await editStatus(submitData)
    ElNotification({ title: '提交成功', type: 'success' })
    emit('success')
    handleClose()
  } catch (error) {
    console.log('审核', error)
  }
}
</script>
<style lang="scss" scoped>
.add-row-box {
  text-align: center;
  margin-top: 20px;
}
</style>
