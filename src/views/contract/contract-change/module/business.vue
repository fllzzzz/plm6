<template>
  <div id="businessContainer" class="app-container">
    <el-form ref="formRef" :model="form" inline size="small" label-position="right" label-width="130px">
      <div>
        <div id="baseInfo">
          <div class="form-row">
            <el-form-item label="合同签订主体" prop="contractSignBodyId">
              <div class="input-underline" style="width: 550px">
                <template v-if="(detail.contractSignBodyName || originContractInfo.contractSignBodyName) && detail.contractSignBodyName!==originContractInfo.contractSignBodyName">
                  <cell-change-preview :old="originContractInfo.contractSignBodyName" :new="detail.contractSignBodyName" />
                </template>
                <span v-else>{{ detail.contractSignBodyName || '-' }}</span>
              </div>
            </el-form-item>
          </div>
          <div class="form-row">
            <el-form-item label="业务类型" prop="businessType">
              <template #label>
                业务类型
                <el-tooltip
                  effect="light"
                  :content="`已创建工作计划时不能修改`"
                  placement="top"
                >
                  <i class="el-icon-info" />
                </el-tooltip>
              </template>
              <div style="width: 200px">
                <template v-if="(detail.businessType || originContractInfo.businessType) && detail.businessType!==originContractInfo.businessType">
                  <cell-change-preview :old="originContractInfo.businessType?businessTypeEnum.VL[originContractInfo.businessType]:'-'" :new="detail.businessType?businessTypeEnum.VL[detail.businessType]:'-'" />
                </template>
                <span v-else>{{ detail.businessType?businessTypeEnum.VL[detail.businessType]:'-' }}</span>
              </div>
            </el-form-item>
            <el-form-item label="项目类型" prop="projectType">
              <div style="width: 200px">
                <template v-if="(detail.projectType || originContractInfo.projectType) && detail.projectType!==originContractInfo.projectType">
                  <cell-change-preview :old="originContractInfo.projectType?projectTypeEnum.VL[originContractInfo.projectType]:'-'" :new="detail.projectType?projectTypeEnum.VL[detail.projectType]:'-'" />
                </template>
                <span v-else>{{ detail.projectType?projectTypeEnum.VL[detail.projectType]:'-' }}</span>
              </div>
            </el-form-item>
            <el-form-item label="项目内容" prop="projectContent">
              <div style="width: 320px">
                <template v-if="(detail.projectContentName || originContractInfo.projectContentName) && detail.projectContentName!==originContractInfo.projectContentName">
                  <cell-change-preview :old="originContractInfo.projectContentName" :new="detail.projectContentName" />
                </template>
                <span v-else>{{ detail.projectContentName || '-' }}</span>
              </div>
            </el-form-item>
          </div>
          <div class="form-row">
            <el-form-item label="签订日期" prop="signingDate">
              <div style="width: 200px">
                <template v-if="(detail.signingDate || originContractInfo.signingDate) && detail.signingDate!==originContractInfo.signingDate">
                  <cell-change-preview :old="originContractInfo.signingDate? parseTime(originContractInfo.signingDate,'{y}-{m}-{d}'): '-'" :new="detail.signingDate? parseTime(detail.signingDate,'{y}-{m}-{d}'): '-'" />
                </template>
                <span v-else>{{ detail.signingDate? parseTime(detail.signingDate,'{y}-{m}-{d}'): '-' }}</span>
              </div>
            </el-form-item>
            <el-form-item label="签约地址" prop="signingAddress">
              <div style="width: 400px">
                <template v-if="(detail.signingAddress || originContractInfo.signingAddress) && detail.signingAddress!==originContractInfo.signingAddress">
                  <cell-change-preview :old="originContractInfo.signingAddress" :new="detail.signingAddress" />
                </template>
                <span v-else class="detail-break">{{ detail.signingAddress }}</span>
              </div>
            </el-form-item>
          </div>
          <div v-if="detail.structureMeasureMode">
            <el-divider><span class="title">结构</span></el-divider>
            <div class="form-row">
              <el-form-item label="结构工程量" prop="quantityWork">
                <template v-if="(detail.quantityWork || originContractInfo.quantityWork) && detail.quantityWork!==originContractInfo.quantityWork">
                  <cell-change-preview :old="originContractInfo.quantityWork" :new="detail.quantityWork" />吨
                </template>
                <span v-else>{{detail.quantityWork}}吨</span>
              </el-form-item>
              <el-form-item label="结构类型" prop="structureStatus">
                <template v-if="(detail.structureStatus || originContractInfo.structureStatus) && detail.structureStatus!==originContractInfo.structureStatus">
                  <cell-change-preview :old="originContractInfo.structureStatus?structureTypeEnum.VL[originContractInfo.structureStatus]:'-'" :new="detail.structureStatus?structureTypeEnum.VL[detail.structureStatus]:'-'" />吨
                </template>
                <span v-else>{{detail.structureStatus?structureTypeEnum.VL[detail.structureStatus]:'-'}}</span>
              </el-form-item>
            </div>
            <div class="form-row">
              <el-form-item label="结构结算方式" prop="structureMeasureMode">
                <template v-if="(isNotBlank(detail.structureMeasureMode) || isNotBlank(originContractInfo.structureMeasureMode)) && originContractInfo.structureMeasureMode!==detail.structureMeasureMode">
                  <cell-change-preview :old="isNotBlank(originContractInfo.structureMeasureMode) ? engineerSettlementTypeEnumN.VL[originContractInfo.structureMeasureMode] : ''" :new="isNotBlank(detail.structureMeasureMode) ? engineerSettlementTypeEnumN.VL[detail.structureMeasureMode] : ''" />
                </template>
                 <span v-else>{{
                  isNotBlank(detail.structureMeasureMode) ? engineerSettlementTypeEnumN.VL[detail.structureMeasureMode] : ''
                }}</span>
              </el-form-item>
              <el-form-item label="结构运输方式" prop="transportMode">
                <template v-if="(isNotBlank(detail.transportMode) || isNotBlank(originContractInfo.transportMode)) && originContractInfo.transportMode!==detail.transportMode">
                  <cell-change-preview :old="isNotBlank(originContractInfo.transportMode) ?transportModeEnum.VL[originContractInfo.transportMode] : ''" :new="isNotBlank(detail.transportMode) ? transportModeEnum.VL[detail.transportMode] : ''" />
                </template>
                 <span v-else>{{
                  isNotBlank(detail.transportMode) ? transportModeEnum.VL[detail.transportMode] : ''
                }}</span>
              </el-form-item>
            </div>
          </div>
          <div class="form-row" v-if="measureModeList && measureModeList.length>0">
            <el-divider><span class="title">围护</span></el-divider>
            <el-form-item label="围护运输方式" prop="enclosureTransportMode">
              <template v-if="(isNotBlank(detail.enclosureTransportMode) || isNotBlank(originContractInfo.enclosureTransportMode)) && originContractInfo.enclosureTransportMode!==detail.enclosureTransportMode">
                  <cell-change-preview :old="isNotBlank(originContractInfo.enclosureTransportMode) ?transportModeEnum.VL[originContractInfo.enclosureTransportMode] : ''" :new="isNotBlank(detail.enclosureTransportMode) ? transportModeEnum.VL[detail.enclosureTransportMode] : ''" />
                </template>
                 <span v-else>{{
                  isNotBlank(detail.enclosureTransportMode) ? transportModeEnum.VL[detail.enclosureTransportMode] : ''
                }}</span>
            </el-form-item>
            <el-form-item label="围护结算方式与工程量" prop="measureModeList" v-if="measureModeList?.length>0" label-width="220px">
              <template v-if="measureModeList.length>0">
                <div v-for="(item,index) in measureModeList" :key="index" style="display:flex;">
                  <span :style="`color:${item.color};`">{{item.typeName}}</span>
                  <span :style="`float:left;width:90px;text-align:right;color:${!isNotBlank(item.originVal)?item.color:''};`">
                    <template v-if="isNotBlank(item.originVal) && !judgeSameValue(item.no,item.originVal.no)">
                        <cell-change-preview :old="TechnologyTypeAllEnum.VL[item.originVal.no]" :new="TechnologyTypeAllEnum.VL[item.no]" />：
                      </template>
                    <template v-else>
                      <span>{{TechnologyTypeAllEnum.VL[item.no]}}：</span>
                    </template>
                  </span>
                  <span :style="`float:left;width:170px;color:${!isNotBlank(item.originVal)?item.color:''};`">
                    <template v-if="isNotBlank(item.originVal) && !judgeSameValue(item.measureMode,item.originVal.measureMode)">
                      <cell-change-preview :old="enclosureSettlementTypeEnum.VL[item.originVal.measureMode]" :new="enclosureSettlementTypeEnum.VL[item.measureMode]" />
                    </template>
                    <template v-else>
                      <span>{{enclosureSettlementTypeEnum.VL[item.measureMode]}}</span>
                    </template>
                  </span>
                  <span :style="`color:${!isNotBlank(item.originVal)?item.color:''};`">
                    工程量：
                    <template v-if="isNotBlank(item.originVal) && !judgeSameValue(item.quantityWork,item.originVal.quantityWork)">
                      <cell-change-preview :old="item.originVal.quantityWork" :new="item.quantityWork" />
                    </template>
                    <template v-else>
                      <span>{{item.quantityWork}}</span>
                    </template>
                  </span>
                  <span :style="`margin-left:2px;color:${!isNotBlank(item.originVal)?item.color:''};`">{{item.measureMode===enclosureSettlementTypeEnum.LENGTH.V?'m':'㎡'}}</span>
                </div>
              </template>
            </el-form-item>
          </div>
          <div class="form-row">
            <el-form-item label="支付方式" prop="payType">
              <div>
                <template v-if="(isNotBlank(detail.payType) || isNotBlank(originContractInfo.payType)) && originContractInfo.payType!==detail.payType">
                  <cell-change-preview :old="isNotBlank(originContractInfo.payType) ?paymentModeEnum.VL[originContractInfo.payType] : ''" :new="isNotBlank(detail.payType) ? paymentModeEnum.VL[detail.payType] : ''" />
                </template>
                 <span v-else>{{
                  isNotBlank(detail.payType) ? paymentModeEnum.VL[detail.payType] : ''
                }}</span>
              </div>
            </el-form-item>
            <el-form-item label="合同含税" prop="isTax">
              <div style="width: 200px">
                <template v-if="(isNotBlank(detail.isTax) || isNotBlank(originContractInfo.isTax)) && originContractInfo.isTax!==detail.isTax">
                  <cell-change-preview :old="isNotBlank(originContractInfo.isTax) ?isTaxContractEnum.VL[originContractInfo.isTax] : ''" :new="isNotBlank(detail.isTax) ? isTaxContractEnum.VL[detail.isTax] : ''" />
                </template>
                 <span v-else>{{
                  isNotBlank(detail.isTax) ? isTaxContractEnum.VL[detail.isTax] : ''
                }}</span>
              </div>
            </el-form-item>
          </div>
           <div class="form-row">
            <el-form-item label="发票类型" prop="invoiceType">
              <div class="input-underline form-row" style="width: 200px">
                <template v-if="(isNotBlank(detail.invoiceType) || isNotBlank(originContractInfo.invoiceType)) && originContractInfo.invoiceType!==detail.invoiceType">
                  <cell-change-preview :old="isNotBlank(originContractInfo.invoiceType) ?invoiceTypeEnum.VL[originContractInfo.invoiceType] : ''" :new="isNotBlank(detail.invoiceType) ? invoiceTypeEnum.VL[detail.invoiceType] : ''" />
                </template>
                 <span v-else>{{
                  detail.invoiceType ? invoiceTypeEnum.VL[detail.invoiceType] : ''
                }}</span>
              </div>
            </el-form-item>
            <el-form-item label="税率" prop="businessTaxRate">
              <template v-if="detail.isTax && detail.invoiceType !== invoiceTypeEnum.RECEIPT.V">
                <template v-if="(isNotBlank(detail.taxRate) || isNotBlank(originContractInfo.taxRate)) && originContractInfo.taxRate!==detail.taxRate">
                  <cell-change-preview :old="originContractInfo.taxRate ? originContractInfo.taxRate*100+'%' : ''" :new="detail.taxRate ? detail.taxRate*100+'%' : ''" />
                </template>
                 <span v-else>{{
                  detail.taxRate ? detail.taxRate*100+'%' : ''
                }}</span>
              </template>
              </el-form-item>
          </div>
          <div class="form-row">
            <el-form-item label="付款方式描述" prop="payTypeDesc">
              <div class="input-underline" style="width: 600px">
                <div style="max-height:220px;overflow-y:auto;word-break:break-all;"><span :style="`color:${originContractInfo.payTypeDesc!==detail.payTypeDesc?'red':''}`">{{ detail.payTypeDesc }}</span></div>
              </div>
            </el-form-item>
          </div>
          <div class="form-row">
            <el-form-item label="技术要求描述" prop="technologyRemark">
              <div class="input-underline" style="width: 600px">
                <div style="max-height:220px;overflow-y:auto;word-break:break-all;"><span :style="`color:${originContractInfo.technologyRemark!==detail.technologyRemark?'red':''}`">{{ detail.technologyRemark }}</span></div>
              </div>
            </el-form-item>
          </div>
        </div>
        <el-divider><span class="title">技术交底</span></el-divider>
        <enclosure-show :table-data="detail.enclosureInfo" :origin-data="originContractInfo.enclosureInfo" :show-item="showItem"/>
      </div>
    </el-form>
  </div>
</template>

<script setup>
import { ref, defineProps, watch, computed } from 'vue'
import {
  projectTypeEnum,
  businessTypeEnum,
  isTaxContractEnum,
  engineerSettlementTypeEnumN,
  enclosureSettlementTypeEnum,
  transportModeEnum,
  TechnologyTypeEnum,
  TechnologyTypeAllEnum,
  structureTypeEnum
} from '@enum-ms/contract'
import { invoiceTypeEnum, paymentModeEnum } from '@enum-ms/finance'
import { isNotBlank } from '@data-type/index'
import EnclosureShow from './enclosure-show'
import { parseTime } from '@/utils/date'
import { judgeSameValue } from '@/views/contract/info/judgeSameValue'

import cellChangePreview from '@comp-common/cell-change-preview'

const formRef = ref()
const showItem = ref([])
const form = ref({})

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
const totalArr = [
  TechnologyTypeEnum.STRUCTURE.V,
  TechnologyTypeEnum.SANDWICH_BOARD.V,
  TechnologyTypeEnum.PROFILED_PLATE.V,
  TechnologyTypeEnum.TRUSS_FLOOR_PLATE.V,
  TechnologyTypeEnum.PRESSURE_BEARING_PLATE.V
]

const measureModeList = computed(() => {
  const arr = []
  props.detail?.measureModeList?.forEach(v => {
    if (isNotBlank(props.originContractInfo?.measureModeList)) {
      if (props.originContractInfo?.measureModeList.findIndex(k => k.no === v.no) > -1) {
        const findVal = props.originContractInfo?.measureModeList.find(k => k.no === v.no)
        if (judgeSameValue(v, findVal)) {
          arr.push({
            ...v,
            typeName: '无变更',
            color: '#909399'
          })
        } else {
          arr.push({
            ...v,
            originVal: findVal,
            typeName: '修改',
            color: '#e6a23c'
          })
        }
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
  props.originContractInfo?.measureModeList?.forEach(v => {
    if (arr.findIndex(k => k.no === v.no) < 0) {
      arr.push({
        ...v,
        typeName: '删除',
        color: 'red'
      })
    }
  })
  return arr
})

watch(
  () => props.detail,
  (val) => {
    if (isNotBlank(val)) {
      showItem.value = []
      if (val.projectContentList.length > 0) {
        val.projectContentList.forEach(v => {
          if (totalArr.indexOf(v.no) > -1 && showItem.value.indexOf(v.no) < 0) {
            showItem.value.push(v.no)
          }
        })
      }
    }
  },
  { deep: true, immediate: true }
)

</script>

<style lang="scss" scoped>
.app-container {
  position: relative;
  .operate-btn {
    position: absolute;
    right: 50px;
    top: 20px;
  }
  .table-box {
    box-sizing: border-box;
    padding: 0 25px;
  }
}
.add-row-box {
  display: flex;
  flex-direction: row;
  justify-content: center;
  align-items: center;
  margin-top: 20px;
}
::v-deep(.input-underline) {
  // width: calc((95vw - 40px)/3);
  width: 200px;
  margin-right: 0;
  input {
    border-top: 0;
    border-left: 0;
    border-right: 0;
    border-radius: 0;
  }
}
// .el-input-number .el-input__inner {
//   text-align: left;
// }
.form-row {
  width: 100%;
}
span {
  // color:#4482ff #1682e6
  color: #82848a;
}
.detail-break{
  word-break:break-all;
}
</style>
