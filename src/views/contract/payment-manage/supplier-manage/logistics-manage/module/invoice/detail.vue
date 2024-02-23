<template>
   <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="handleClose"
    v-model="visible"
    title="收票申请详情"
    :wrapper-closable="false"
    size="60%"
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
      <el-form ref="formRef" size="small" label-width="130px" style="position:relative;">
         <el-row>
          <el-col :span="12">
            <el-form-item label="购买方" prop="paymentUnitId">
              <span>{{currentRow.branchCompanyName}}</span>
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="销售方" prop="supplierName">
              <span>{{ currentRow.supplierName }}</span>
            </el-form-item>
          </el-col>
        </el-row>
        <el-row>
          <el-col :span="12">
            <el-form-item label="发票及税率" prop="invoiceTypeEnum">
              <span>{{invoiceTypeEnum.VL[currentRow.invoiceType]}}【{{currentRow.invoiceType !== invoiceTypeEnum.RECEIPT.V?(currentRow.checked?'免税':(currentRow.taxRate)+'%'):''}}】</span>
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="实际开票单位" prop="actualReceivingUnitId">
              <span>{{currentRow.actualInvoiceUnit}}</span>
            </el-form-item>
          </el-col>
        </el-row>
        <el-row>
          <el-col :span="12">
            <el-form-item label="发票面额" prop="invoiceAmount">
              <span>{{toThousand(currentRow.invoiceAmount)}}</span>
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="发票号" prop="invoiceSerialNumber">
              <span>{{currentRow.invoiceSerialNumber}}</span>
            </el-form-item>
          </el-col>
        </el-row>
        <el-row>
          <el-col :span="12">
            <el-form-item label="收票日期" prop="receiveInvoiceDate">
              <span>{{parseTime(currentRow.receiveInvoiceDate,'{y}-{m}-{d}')}}</span>
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="开票日期" prop="createInvoiceDate">
              <el-date-picker
                v-if="showType==='audit'"
                v-model="createInvoiceDate"
                type="date"
                size="small"
                value-format="x"
                placeholder="选择日期"
                :disabledDate="(date) => {return date.getTime() > new Date().getTime()}"
                style="width:280px;"
              />
              <span v-else>{{parseTime(currentRow.createInvoiceDate,'{y}-{m}-{d}')}}</span>
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
            <el-form-item label="发票内容" prop="invoiceContent">
              <el-input
                v-if="showType==='audit'"
                v-model="invoiceContent"
                type="textarea"
                style="width: 280px"
                maxlength="200"
                show-word-limit
                :autosize="{ minRows: 2, maxRows: 4 }"
                placeholder="请输入发票内容"
              />
              <span v-else>{{currentRow.invoiceContent}}</span>
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
            <el-form-item label="发票凭证">
              <template #label>
                发票凭证
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
                  <div style="cursor:pointer;color:#409eff;" @dblclick="attachmentView(item)">{{item.name}}</div>
                </div>
              </template>
            </el-form-item>
          </el-col>
        </el-row>
        <div v-if="currentRow.auditStatus===auditTypeEnum.PASS.V" style="position:absolute;top:50%;left:50%;transform:translateX(-50%) translateY(-50%);">
          <span style="font-size:26px;color:red;padding:10px 50px;border:1px solid red;">已审核</span>
        </div>
      </el-form>
      <showPdfAndImg v-if="pdfShow" :isVisible="pdfShow" :showType="'attachment'" :id="currentId" @close="pdfShow=false"/>
    </template>
  </common-drawer>
</template>

<script setup>
import { audit } from '@/api/contract/supplier-manage/jd-logistics-invoice'
import { ref, defineProps, defineEmits, watch } from 'vue'
import { ElMessageBox, ElNotification } from 'element-plus'

import { invoiceTypeEnum } from '@enum-ms/finance'
import { toThousand } from '@data-type/number'
import { parseTime } from '@/utils/date'
import useVisible from '@compos/use-visible'
import { auditTypeEnum } from '@enum-ms/contract'
import showPdfAndImg from '@comp-base/show-pdf-and-img.vue'

const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

const formRef = ref()
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
const createInvoiceDate = ref()
const invoiceContent = ref()

watch(
  () => props.currentRow,
  (val) => {
    if (val) {
      createInvoiceDate.value = props.currentRow.createInvoiceDate
      invoiceContent.value = props.currentRow.invoiceContent
    }
  },
  { deep: true, immediate: true }
)

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
    await audit({ id: props.currentRow.id, auditStatus: val, createInvoiceDate: createInvoiceDate.value, invoiceContent: invoiceContent.value })
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
