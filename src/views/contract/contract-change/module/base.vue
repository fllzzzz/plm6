<template>
  <div id="baseContainer" class="app-container">
    <el-form
      ref="formRef"
      :model="form"
      inline
      size="small"
      label-position="right"
      label-width="104px"
    >
      <div>
        <div class="form-row">
          <el-form-item label="合同编号" prop="serialNumber">
            <div class="input-underline">
              <span>{{ detail.serialNumber || '-' }}</span>
            </div>
          </el-form-item>
          <el-form-item label="项目名称" prop="name">
            <div class="input-underline">
              <span>{{ detail.name || '-' }}</span>
            </div>
          </el-form-item>
          <el-form-item label="订单来源" prop="orderSourceType">
            <div class="input-underline">
              <span>{{ detail.orderSourceType? orderSourceTypeEnum.VL[detail.orderSourceType]: '-' }}</span>
            </div>
          </el-form-item>
        </div>
        <div>
          <el-form-item label="开工时间" prop="startDate">
            <div class="input-underline">
              <span>{{ detail.startDate? parseTime(detail.startDate,'{y}-{m}-{d}'): '-' }}</span>
            </div>
          </el-form-item>
          <el-form-item label="项目简称" prop="shortName">
            <div class="input-underline">
              <span>{{ detail.shortName || '-' }}</span>
            </div>
          </el-form-item>
        </div>
        <div class="form-row">
          <el-form-item label="完成时间" prop="endDate">
            <div class="input-underline">
              <span>{{ detail.endDate? parseTime(detail.endDate,'{y}-{m}-{d}'): '-' }}</span>
            </div>
          </el-form-item>
          <el-form-item label="总工期(天)" prop="totalDuration">
            <div class="input-underline">
              <span>{{ detail.startDate && detail.endDate? dateDifference(detail.startDate, detail.endDate): '-' }}</span>
            </div>
          </el-form-item>
        </div>
        <div class="form-row">
          <el-form-item label="项目省市区" prop="region">
            <div class="input-underline">
              <span>{{ detail.regionalFullName || '-' }}</span>
            </div>
          </el-form-item>
          <el-form-item label="详细地址" prop="address">
            <div class="input-underline" style="width:420px">
              <span class="detail-break">{{ props.detail.address || '-' }}</span>
            </div>
          </el-form-item>
        </div>
        <el-divider><span class="title">负责人</span></el-divider>
        <div class="form-row">
          <el-form-item label="项目经理" prop="projectManagerId">
            <div class="input-underline" style="width:300px">
              <span>{{ detail.projectManagerFullName || '-' }}</span>
            </div>
          </el-form-item>
          <el-form-item label="销售负责人" prop="projectManagerId">
            <div class="input-underline" style="width:300px">
              <span>{{ detail.singerName || '-' }}</span>
            </div>
          </el-form-item>
        </div>
        <el-divider><span class="title">合同金额</span></el-divider>
        <div class="form-row">
          <el-form-item label="合同金额(元)" prop="contractAmount">
            <div class="input-underline">
              <span>{{ detail.contractAmount? toThousand(detail.contractAmount,decimalPrecision.contract): '-' }}</span>
              <div style="color:#82848a">{{ detail.contractAmount? digitUppercase(detail.contractAmount):'' }}</div>
            </div>
          </el-form-item>
          <el-form-item label="预付款(元)" prop="prepayments">
            <div class="input-underline">
              <span>{{ detail.prepayments? toThousand(detail.prepayments,decimalPrecision.contract): '' }}</span>
            </div>
          </el-form-item>
          <el-form-item label="管理费(元)" prop="managementFeeRate">
            <span>{{detail.managementFeeRate && detail.contractAmount ?  toThousand((detail.managementFeeRate * detail.contractAmount / 100),decimalPrecision.contract) : '-'}}</span>
            <span>（费率:{{ detail.managementFeeRate ? detail.managementFeeRate.toFixed(DP.ACCOUNTING): '-' }}%）</span>
          </el-form-item>
        </div>
        <div class="form-row">
          <el-form-item label="保证金(元)" prop="marginAmount">
            <div class="input-underline">
              <span>{{ detail.marginAmount? toThousand(detail.marginAmount,decimalPrecision.contract): '-' }}</span>
            </div>
          </el-form-item>
          <el-form-item label="保证金类型" prop="marginType">
            <div class="input-underline">
              <span>{{ detail.marginType && dict && dict.label && dict.label['margin_type']? dict.label['margin_type'][detail.marginType]: '-' }}</span>
            </div>
          </el-form-item>
          <el-form-item label="币种" prop="currencyType">
            <div class="input-underline">
              <span>{{ detail.currencyType && dict && dict.label && dict.label['currency_type']? dict.label['currency_type'][detail.currencyType]: '-'}}</span>
            </div>
          </el-form-item>
        </div>
      </div>
      <el-divider><span class="title">合同附件</span></el-divider>
      <div class="table-box">
        <upload-list
          :show-download="true"
          showView
          :file-classify="fileClassifyEnum.CONTRACT_ATT.V"
          v-model:files="detail.attachments"
          :download-fn="downloadBaseAttachments"
          :uploadable="false"
          empty-text="暂未上传合同附件"
        />
      </div>
    </el-form>
  </div>
</template>

<script setup>
import { ref, defineProps } from 'vue'

import { dateDifference } from '@/utils/date'
import useDict from '@compos/store/use-dict'
import { fileClassifyEnum } from '@enum-ms/file'
import { orderSourceTypeEnum } from '@enum-ms/contract'
import { DP } from '@/settings/config'
import { downloadBaseAttachments } from '@/api/contract/project'
import { parseTime } from '@/utils/date'
import { toThousand } from '@data-type/number'
import { digitUppercase } from '@/utils/data-type/number'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

import uploadList from '@comp/file-upload/UploadList.vue'

const formRef = ref()
const dict = useDict(['margin_type', 'currency_type'])
const form = ref({})

const { decimalPrecision } = useDecimalPrecision()

const props = defineProps({
  detail: {
    type: Object,
    default: () => {}
  }
})

</script>
<style lang="scss" scoped>
.app-container{
  position: relative;
  .operate-btn {
    position: absolute;
    right: 50px;
    top: 20px;
  }
}
.table-box {
  box-sizing: border-box;
  padding: 0 25px;
}
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
::v-deep(.input-underline) {
  // width: calc((95vw - 40px)/3);
  width: 320px;
  margin-right: 0;
  input{
    border-top:0;
    border-left:0;
    border-right:0;
    border-radius: 0;
  }
}
.form-row {
  width:100%
}
span {
  // color:#4482ff #1682e6
  color:#82848a
}
.detail-break{
  word-break:break-all;
}
</style>
