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
              <template v-if="detail.serialNumber!==originContractInfo.serialNumber">
                <cell-change-preview :old="originContractInfo.serialNumber" :new="detail.serialNumber" />
              </template>
              <span v-else>{{ detail.serialNumber || '-' }}</span>
            </div>
          </el-form-item>
          <el-form-item label="项目名称" prop="name">
            <div class="input-underline">
              <template v-if="detail.name!==originContractInfo.name">
                <cell-change-preview :old="originContractInfo.name" :new="detail.name" />
              </template>
              <span v-else>{{ detail.name || '-' }}</span>
            </div>
          </el-form-item>
          <el-form-item label="订单来源" prop="orderSourceType">
            <div class="input-underline">
              <template v-if="detail.orderSourceType!==originContractInfo.orderSourceType">
                <cell-change-preview :old="orderSourceTypeEnum.VL[originContractInfo.orderSourceType]" :new="orderSourceTypeEnum.VL[detail.orderSourceType]" />
              </template>
              <span v-else>{{ detail.orderSourceType? orderSourceTypeEnum.VL[detail.orderSourceType]: '-' }}</span>
            </div>
          </el-form-item>
        </div>
        <div>
          <el-form-item label="开工时间" prop="startDate">
            <div class="input-underline">
              <template v-if="detail.startDate!==originContractInfo.startDate">
                <cell-change-preview :old="originContractInfo.startDate? parseTime(originContractInfo.startDate,'{y}-{m}-{d}'): '-'" :new="detail.startDate? parseTime(detail.startDate,'{y}-{m}-{d}'): '-'" />
              </template>
              <span v-else>{{ detail.startDate? parseTime(detail.startDate,'{y}-{m}-{d}'): '-' }}</span>
            </div>
          </el-form-item>
          <el-form-item label="项目简称" prop="shortName">
            <div class="input-underline">
              <template v-if="detail.shortName!==originContractInfo.shortName">
                <cell-change-preview :old="originContractInfo.shortName" :new="detail.shortName" />
              </template>
              <span v-else>{{ detail.shortName || '-' }}</span>
            </div>
          </el-form-item>
        </div>
        <div class="form-row">
          <el-form-item label="完成时间" prop="endDate">
            <div class="input-underline">
              <template v-if="detail.endDate!==originContractInfo.endDate">
                <cell-change-preview :old="originContractInfo.endDate? parseTime(originContractInfo.endDate,'{y}-{m}-{d}'): '-'" :new="detail.endDate? parseTime(detail.endDate,'{y}-{m}-{d}'): '-'" />
              </template>
              <span v-else>{{ detail.endDate? parseTime(detail.endDate,'{y}-{m}-{d}'): '-' }}</span>
            </div>
          </el-form-item>
          <el-form-item label="总工期(天)" prop="totalDuration">
            <div class="input-underline">
              <template v-if="dateDifference(originContractInfo.startDate, originContractInfo.endDate)!==dateDifference(detail.startDate, detail.endDate)">
                <cell-change-preview :old="dateDifference(originContractInfo.startDate, originContractInfo.endDate)" :new="dateDifference(detail.startDate, detail.endDate)" />
              </template>
              <span v-else>{{ detail.startDate && detail.endDate? dateDifference(detail.startDate, detail.endDate): '-' }}</span>
            </div>
          </el-form-item>
        </div>
        <div class="form-row">
          <el-form-item label="项目省市区" prop="region">
            <div class="input-underline">
              <template v-if="(detail.regionalFullName|| originContractInfo.regionalFullName) && detail.regionalFullName!==originContractInfo.regionalFullName">
                <cell-change-preview :old="originContractInfo.regionalFullName" :new="detail.regionalFullName" />
              </template>
              <span v-else>{{ detail.regionalFullName || '-' }}</span>
            </div>
          </el-form-item>
          <el-form-item label="详细地址" prop="address">
            <div class="input-underline" style="width:420px">
              <template v-if="(detail.address|| originContractInfo.address) && detail.address!==originContractInfo.address">
                <cell-change-preview :old="originContractInfo.address" :new="detail.address" />
              </template>
              <span v-else>{{ detail.address || '-' }}</span>
            </div>
          </el-form-item>
        </div>
        <el-divider><span class="title">负责人</span></el-divider>
        <div class="form-row">
          <el-form-item label="项目经理" prop="projectManagerId">
            <div class="input-underline" style="width:300px">
              <template v-if="(detail.projectManagerFullName|| originContractInfo.projectManagerFullName) && detail.projectManagerFullName!==originContractInfo.projectManagerFullName">
                <cell-change-preview :old="originContractInfo.projectManagerFullName" :new="detail.projectManagerFullName" />
              </template>
              <span v-else>{{ detail.projectManagerFullName || '-' }}</span>
            </div>
          </el-form-item>
          <el-form-item label="销售负责人" prop="projectManagerId">
            <div class="input-underline" style="width:300px">
              <template v-if="(detail.signerName || originContractInfo.signerName) && detail.signerName!==originContractInfo.signerName">
                <cell-change-preview :old="originContractInfo.signerName" :new="detail.signerName" />
              </template>
              <span v-else>{{ detail.signerName || '-' }}</span>
            </div>
          </el-form-item>
          <el-form-item label="所属部门" prop="relationDeptId">
            <span>{{ detail.relationDeptName || '-' }}</span>
          </el-form-item>
        </div>
        <el-divider><span class="title">合同金额</span></el-divider>
        <div class="form-row">
           <el-form-item label="签约额(元)" prop="signAmount">
           <div class="input-underline">
              <span>{{ detail.signAmount? toThousand(detail.signAmount,decimalPrecision.contract): '-' }}</span>
              <div style="color:#82848a">{{ detail.signAmount? digitUppercase(detail.signAmount):'' }}</div>
            </div>
          </el-form-item>
          <el-form-item label="合同金额(元)" prop="contractAmount">
            <div class="input-underline">
              <template v-if="detail.contractAmount!==originContractInfo.contractAmount">
                <cell-change-preview :old="toThousand(originContractInfo.contractAmount,decimalPrecision.contract)" :new="toThousand(detail.contractAmount,decimalPrecision.contract)" />
              </template>
              <div v-else>
                <span>{{ detail.contractAmount? toThousand(detail.contractAmount,decimalPrecision.contract): '-' }}</span>
                <div style="color:#82848a">{{ detail.contractAmount? digitUppercase(detail.contractAmount):'' }}</div>
              </div>
            </div>
          </el-form-item>
        </div>
        <div class="form-row">
          <el-form-item label="预付款(元)" prop="prepayments">
            <div class="input-underline">
              <template v-if="(detail.prepayments || originContractInfo.prepayments) && detail.prepayments!==originContractInfo.prepayments">
                <cell-change-preview :old="toThousand(originContractInfo.prepayments,decimalPrecision.contract)" :new="toThousand(detail.prepayments,decimalPrecision.contract)" />
              </template>
              <div v-else>
                <span>{{ detail.prepayments? toThousand(detail.prepayments,decimalPrecision.contract): '-' }}</span>
              </div>
            </div>
          </el-form-item>
          <el-form-item label="管理费(元)" prop="managementFeeRate">
              <template v-if="(detail.managementFeeRate || originContractInfo.managementFeeRate) && detail.managementFeeRate!==originContractInfo.managementFeeRate">
                <cell-change-preview :old="toThousand((originContractInfo.managementFeeRate * originContractInfo.contractAmount / 100),decimalPrecision.contract)" :new="toThousand((detail.managementFeeRate * detail.contractAmount / 100),decimalPrecision.contract)" />
                （费率:<cell-change-preview :old="originContractInfo.managementFeeRate+'%'" :new="detail.managementFeeRate+'%'" />)
              </template>
              <template v-else>
                <span>{{detail.managementFeeRate && detail.contractAmount ?  toThousand((detail.managementFeeRate * detail.contractAmount / 100),decimalPrecision.contract) : '-'}}</span>
                <span>（费率:{{ detail.managementFeeRate ? detail.managementFeeRate.toFixed(DP.ACCOUNTING): '-' }}%）</span>
              </template>
          </el-form-item>
        </div>
        <div class="form-row">
          <el-form-item label="保证金(元)" prop="marginAmount">
            <div class="input-underline">
              <template v-if="(detail.marginAmount || originContractInfo.marginAmount) && detail.marginAmount!==originContractInfo.marginAmount">
                <cell-change-preview :old="toThousand(originContractInfo.marginAmount,decimalPrecision.contract)" :new="toThousand(detail.marginAmount,decimalPrecision.contract)" />
              </template>
              <div v-else>
                <span>{{ detail.marginAmount? toThousand(detail.marginAmount,decimalPrecision.contract): '-' }}</span>
              </div>
            </div>
          </el-form-item>
          <el-form-item label="保证金类型" prop="marginType">
            <div class="input-underline">
              <template v-if="((detail.marginType || originContractInfo.marginType) && detail.marginType!==originContractInfo.marginType)">
                <cell-change-preview :old="originContractInfo.marginType && dict && dict.label && dict.label['margin_type']? dict.label['margin_type'][originContractInfo.marginType]: '-'" :new="detail.marginType && dict && dict.label && dict.label['margin_type']? dict.label['margin_type'][detail.marginType]: '-'" />
              </template>
              <span v-else>{{ detail.marginType && dict && dict.label && dict.label['margin_type']? dict.label['margin_type'][detail.marginType]: '-' }}</span>
            </div>
          </el-form-item>
          <el-form-item label="币种" prop="currencyType">
            <div class="input-underline">
              <template v-if="((detail.currencyType || originContractInfo.currencyType) && detail.currencyType!==originContractInfo.currencyType)">
                <cell-change-preview :old="originContractInfo.currencyType && dict && dict.label && dict.label['currency_type']? dict.label['currency_type'][originContractInfo.currencyType]: '-'" :new="detail.currencyType && dict && dict.label && dict.label['currency_type']? dict.label['currency_type'][detail.currencyType]: '-'" />
              </template>
              <span v-else>{{ props.detail.currencyType && dict && dict.label && dict.label['currency_type']? dict.label['currency_type'][detail.currencyType]: '-' }}</span>
            </div>
          </el-form-item>
        </div>
      </div>
    </el-form>
    <el-divider><span class="title">合同附件</span></el-divider>
      <div class="table-box">
        <common-table ref="tableRef" :data="attachmentFile" return-source-data :showEmptySymbol="false" :max-height="300" >
          <el-table-column key="typeName" prop="typeName" label="变更类型" align="center" width="100">
            <template #default="{ row }">
              <span :style="`color:${row?.color}`">{{row.typeName}}</span>
            </template>
          </el-table-column>
          <el-table-column key="schedulingQuantity" prop="schedulingQuantity" label="名称" align="left">
            <template #default="{ row }">
              <span :style="`color:${row?.color}`">{{row.name}}</span>
            </template>
          </el-table-column>
          <el-table-column prop="createTime" label="上传时间" :show-overflow-tooltip="true" width="100" align="center">
            <template #default="{ row }">
              <span  :style="`color:${row?.color}`" v-parse-time="{ val: row?.createTime, fmt: '{y}-{m}-{d}' }" />
            </template>
          </el-table-column>
          <el-table-column label="操作" align="center" width="120">
            <template #default="{ row, $index }">
              <template v-if="row?.typeName!=='删除'">
                 <export-button
                  :params="getParams(row, $index)"
                  :fn="downloadBaseAttachments"
                />
                <common-button icon="el-icon-view" size="mini" @click="attachmentView(row)" />
              </template>
            </template>
          </el-table-column>
        </common-table>
      </div>
    <showPdfAndImg v-if="pdfShow" :isVisible="pdfShow" :showType="'attachment'" :id="currentId" @close="pdfShow = false" />
  </div>
</template>

<script setup>
import { ref, defineProps, computed } from 'vue'

import { isNotBlank } from '@data-type/index'
import { dateDifference } from '@/utils/date'
import useDict from '@compos/store/use-dict'
import { orderSourceTypeEnum } from '@enum-ms/contract'
import { DP } from '@/settings/config'
import { downloadBaseAttachments } from '@/api/contract/project'
import { parseTime } from '@/utils/date'
import { toThousand } from '@data-type/number'
import { digitUppercase } from '@/utils/data-type/number'
import useDecimalPrecision from '@compos/store/use-decimal-precision'
import cellChangePreview from '@comp-common/cell-change-preview'

import showPdfAndImg from '@comp-base/show-pdf-and-img.vue'
import ExportButton from '@comp-common/export-button/index.vue'

const formRef = ref()
const dict = useDict(['margin_type', 'currency_type'])
const form = ref({})
const pdfShow = ref(false)
const currentId = ref()

const { decimalPrecision } = useDecimalPrecision()

const props = defineProps({
  detail: {
    type: Object,
    default: () => {}
  },
  originContractInfo: {
    type: Object,
    default: () => {}
  }
})

// 预览附件
function attachmentView(row) {
  if (row?.name.indexOf('.pdf') > -1 || row?.name.indexOf('.png') > -1 || row?.name.indexOf('.jpg') > -1 || row?.name.indexOf('.jpeg') > -1) {
    currentId.value = row.id
    pdfShow.value = true
  }
}

const attachmentFile = computed(() => {
  const arr = []
  props.detail?.attachments?.forEach(v => {
    if (isNotBlank(props.originContractInfo?.attachments)) {
      if (props.originContractInfo?.attachments.findIndex(k => k.id === v.id) > -1) {
        arr.push({
          ...v,
          typeName: '无变更',
          color: '#909399'
        })
      } else {
        arr.push({
          ...v,
          typeName: '新增',
          color: 'green'
        })
      }
    } else {
      arr.push({
        ...v,
        typeName: '新增',
        color: 'green'
      })
    }
  })
  props.originContractInfo?.attachments?.forEach(v => {
    if (arr.findIndex(k => k.id === v.id) < 0) {
      arr.push({
        ...v,
        typeName: '删除',
        color: 'red'
      })
    }
  })
  return arr
})

function getParams(row, index) {
  const id = index >= 0 && index <= attachmentFile.value.length - 1 ? attachmentFile.value[index].id : undefined
  return { ...props.downloadParams, id }
}

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
