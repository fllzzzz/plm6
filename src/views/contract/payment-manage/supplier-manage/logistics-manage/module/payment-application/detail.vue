<template>
   <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="handleClose"
    v-model="visible"
    title="付款申请详情"
    :wrapper-closable="false"
    size="40%"
  >
    <template #titleAfter>
      <el-tag v-if="currentRow.auditStatus" size="medium" :type="currentRow.auditStatus===auditTypeEnum.REJECT.V?'info':(currentRow.auditStatus===auditTypeEnum.PASS.V?'success':'warning')">
        {{ currentRow.auditStatus===auditTypeEnum.REJECT.V?'已驳回':(currentRow.auditStatus===auditTypeEnum.PASS.V?'已通过':'审核中') }}
      </el-tag>
      <!--  -->
    </template>
    <template #titleRight>
      <template v-if="showType==='audit' && currentRow.auditStatus===auditTypeEnum.AUDITING.V">
        <common-button size="small" type="info" @click="passConfirm(auditTypeEnum.ENUM.REJECT.V)">驳回</common-button>
        <common-button size="small" type="success" @click="passConfirm(auditTypeEnum.ENUM.PASS.V)">通过</common-button>
      </template>
    </template>
    <template #content>
      <el-form ref="formRef" size="small" label-width="130px">
        <el-row>
          <el-col :span="12">
            <el-form-item label="供应商">
              <span>{{ detailInfo.supplierName }}</span>
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="累计入库额">
              <span>{{toThousand(detailInfo.inboundAmount)}}</span>
            </el-form-item>
          </el-col>
        </el-row>
        <el-row>
          <el-col :span="12">
            <el-form-item label="实际收款单位">
              <span>{{currentRow.actualReceivingUnit}}</span>
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="累计付款">
              <span>{{toThousand(detailInfo.paymentAmount)}}</span><span>（{{ detailInfo.paymentRate }}%）</span>
            </el-form-item>
          </el-col>
        </el-row>
         <el-row>
          <el-col :span="12">
            <el-form-item label="累计收票">
              <span v-thousand="detailInfo.invoiceAmount"/><span>（{{ detailInfo.invoiceRate }}%）</span>
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="最后一次付款">
              <span  v-thousand="detailInfo.lastPaymentAmount" />
              <el-tag v-if="detailInfo.lastPaymentTime" style="margin-left:5px;">{{ parseTime(detailInfo.lastPaymentTime,'{y}-{m}-{d}')}}</el-tag>
            </el-form-item>
          </el-col>
        </el-row>
         <el-row>
          <el-col :span="12">
            <el-form-item label="付款日期" prop="paymentDate">
              <span v-if="currentRow.paymentDate" style="margin-left:5px;">{{ parseTime(currentRow.paymentDate,'{y}-{m}-{d}')}}</span>
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="本次付款"  prop="applyAmount">
              <span v-thousand="currentRow.applyAmount"/>
            </el-form-item>
          </el-col>
        </el-row>
         <el-row>
          <el-col :span="12">
            <el-form-item label="付款事由" prop="paymentReasonId">
              <div>{{ currentRow.paymentReasonId && dict && dict.label && dict.label['payment_reason']? dict.label['payment_reason'][ currentRow.paymentReasonId]: '' }}</div>
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="大写">
              <span style="color:#82848a">{{currentRow?.sourceRow?.applyAmount?digitUppercase(currentRow?.sourceRow?.applyAmount):''}}</span>
            </el-form-item>
          </el-col>
        </el-row>
         <el-row>
          <el-col :span="12">
            <el-form-item label="付款单位" prop="paymentUnitId">
              <span>{{currentRow.paymentUnit}}</span>
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="付款行" prop="paymentBank">
              <span>{{currentRow.paymentBank}}</span>
            </el-form-item>
          </el-col>
        </el-row>
         <el-row>
          <el-col :span="12">
            <el-form-item label="备注" prop="remark">
              <span>{{currentRow.remark}}</span>
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="账号">
              <span>{{currentRow.paymentBankAccount}}</span>
            </el-form-item>
          </el-col>
        </el-row>
        <el-row>
          <el-col :span="12">
            <el-form-item label="操作人">
              <span>{{currentRow.applyUserName}}{{parseTime(currentRow.createTime,'{y}-{m}-{d}')}}</span>
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="审核人">
              <span>{{currentRow.auditUserName}}{{currentRow.auditTime?parseTime(currentRow.auditTime,'{y}-{m}-{d}'):''}}</span>
             </el-form-item>
          </el-col>
        </el-row>
        <el-row>
          <el-col :span="12">
            <el-form-item label="付款凭证">
              <template #label>
                付款凭证
                <el-tooltip
                  effect="light"
                  :content="`双击可预览附件`"
                  placement="top"
                  v-if="currentRow.attachments?.length"
                >
                  <i class="el-icon-info" />
                </el-tooltip>
              </template>
              <template v-if="props.currentRow.attachments?.length">
                <div v-for="item in currentRow.attachments" :key="item.id">
                  <div style="cursor:pointer;" @dblclick="attachmentView(item)">{{item.name}}</div>
                </div>
              </template>
            </el-form-item>
          </el-col>
        </el-row>
      </el-form>
      <showPdfAndImg v-if="pdfShow" :isVisible="pdfShow" :showType="'attachment'" :id="currentId" @close="pdfShow=false"/>
    </template>
  </common-drawer>
</template>

<script setup>
import { audit } from '@/api/contract/supplier-manage/jd-logistics-payment'
import { ref, defineProps, defineEmits } from 'vue'
import { ElMessageBox, ElNotification } from 'element-plus'

import { toThousand, digitUppercase } from '@data-type/number'
import { parseTime } from '@/utils/date'
import useVisible from '@compos/use-visible'
import useDict from '@compos/store/use-dict'
import { auditTypeEnum } from '@enum-ms/contract'
import showPdfAndImg from '@comp-base/show-pdf-and-img.vue'

const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

const formRef = ref()
const dict = useDict(['payment_reason'])
const props = defineProps({
  detailInfo: {
    type: Object,
    default: () => {}
  },
  currentRow: {
    type: Object,
    default: () => {}
  },
  showType: {
    type: String,
    default: 'detail'
  }
})

const pdfShow = ref(false)
const currentId = ref()

// 预览附件
function attachmentView(item) {
  currentId.value = item.id
  pdfShow.value = true
}

async function passConfirm(val) {
  try {
    const title = val === auditTypeEnum.PASS.V ? '通过' : '驳回'
    await ElMessageBox.confirm('该操作为最终审核,确认后将无法撤回', title, {
      confirmButtonText: '是',
      cancelButtonText: '否',
      type: 'warning'
    })
    await audit(props.currentRow.id, val)
    ElNotification({ title: title + '成功', type: 'success' })
    handleClose()
    emit('success')
  } catch (e) {
    console.log('审核失败', e)
  }
}

</script>
<style lang="scss" scoped>
.add-row-box {
  text-align: center;
  margin-top: 20px;
}
</style>
