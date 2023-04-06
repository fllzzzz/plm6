<template>
  <common-dialog
    append-to-body
    :visible="crud.detailVisible"
    :content-loading="crud.detailLoading"
    :before-close="crud.cancelDetail"
    title="分包订单详情"
    :show-close="true"
    width="600px"
  >
    <el-form ref="formRef" size="small" label-width="120px" label-position="right">
      <el-form-item label="订单编号" prop="serialNumber">
        <span>{{detail.serialNumber}}</span>
      </el-form-item>
      <el-form-item label="签订日期" prop="signDate">
        <span v-parse-time="{ val: detail.signDate, fmt: '{y}-{m}-{d}' }" />
      </el-form-item>
      <el-form-item label="项目" prop="projectId">
        <span class="project-name">{{projectNameFormatter(detail.project)}}</span>
      </el-form-item>
      <el-form-item label="包含单体" prop="monomerIds">
       <span v-for="item in detail.monomerList" :key="item.id">{{`【${item.name}】`}}</span>
      </el-form-item>
      <el-form-item label="分包单位" prop="supplierId">
        <span>{{detail.supplierName}}</span>
      </el-form-item>
      <el-form-item label="分包类别" prop="subcontractClassId">
        <span>{{detail.subcontractClassName}}</span>
      </el-form-item>
      <el-form-item label="合同额" prop="amount">
        <span v-thousand="detail.amount" v-empty-text />
      </el-form-item>
      <el-form-item label="发票类型" prop="invoiceType">
        <span>{{invoiceTypeEnum.VL[detail.invoiceType]}}</span>
      </el-form-item>
      <el-form-item label="税率" prop="taxRate">
        <span>{{detail.invoiceType!==invoiceTypeEnum.RECEIPT.V?detail.taxRate:'-'}}%</span>
      </el-form-item>
      <el-form-item label="附件">
        <template #label>
          附件
          <el-tooltip
            effect="light"
            :content="`双击可预览附件`"
            placement="top"
            v-if="detail.attachments?.length"
          >
            <i class="el-icon-info" />
          </el-tooltip>
        </template>
        <template v-if="detail.attachments?.length">
          <div v-for="item in detail.attachments" :key="item.id">
            <div style="cursor:pointer;" @dblclick="attachmentView(item)">{{item.name}}</div>
          </div>
        </template>
      </el-form-item>
      <el-form-item label="备注" prop="remark">
        <span>{{detail.remark}}</span>
      </el-form-item>
    </el-form>
    <showPdfAndImg v-if="pdfShow" :isVisible="pdfShow" :showType="'attachment'" :id="currentId" @close="pdfShow=false"/>
  </common-dialog>
</template>

<script setup>
import { ref } from 'vue'
import { regDetail } from '@compos/use-crud'

import { invoiceTypeEnum } from '@enum-ms/finance'
import { projectNameFormatter } from '@/utils/project'
import showPdfAndImg from '@comp-base/show-pdf-and-img.vue'

const pdfShow = ref(false)
const currentId = ref()

const { crud, detail } = regDetail()

// 预览附件
function attachmentView(item) {
  currentId.value = item.id
  pdfShow.value = true
}

</script>

<style lang="scss" scoped>
.raw-mat-inbound-application-record-detail {
  .el-table {
    ::v-deep(td .cell) {
      min-height: 28px;
      line-height: 28px;
    }
  }
}
</style>
