<template>
  <div id="businessContainer" class="app-container">
    <el-form ref="formRef" :model="form" inline size="small" label-position="right" label-width="130px">
      <div>
        <div id="baseInfo">
          <div class="form-row">
            <el-form-item label="合同签订主体" prop="contractSignBodyId">
              <div class="input-underline" style="width: 550px">
                <span >{{ detail.signingMainBodyName }}</span>
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
                <span>{{ detail.businessType?businessTypeEnum.VL[detail.businessType]:'-' }}</span>
              </div>
            </el-form-item>
            <el-form-item label="项目内容" prop="projectContent">
              <div style="width: 320px">
                <span v-for="item in detail.projectContentList" :key="item.id">{{ item.name }}&nbsp;</span>
              </div>
            </el-form-item>
          </div>
          <div class="form-row">
            <el-form-item label="项目类型" prop="projectType">
              <div style="width: 200px">
                <span>{{ detail.projectType?projectTypeEnum.VL[detail.projectType]:'-' }}</span>
              </div>
            </el-form-item>
            <el-form-item label="签约人" prop="signerId">
              <div style="width: 200px">
                <span>{{ detail.signerName }}</span>
              </div>
            </el-form-item>
          </div>
          <div class="form-row">
            <el-form-item label="签订日期" prop="signingDate">
              <div style="width: 200px">
                <span v-if="detail.signingDate">{{ parseTime(detail.signingDate,'{y}-{m}-{d}') }}</span>
              </div>
            </el-form-item>
            <el-form-item label="签约地址" prop="signingAddress">
              <div style="width: 400px">
                <span class="detail-break">{{ detail.signingAddress }}</span>
              </div>
            </el-form-item>
          </div>
          <div class="form-row">
            <el-form-item label="工程结算方式" prop="structureMeasureMode">
              <div style="width: 200px">
                <span>{{
                  isNotBlank(detail.structureMeasureMode) ? engineerSettlementTypeEnumN.VL[detail.structureMeasureMode] : ''
                }}</span>
              </div>
            </el-form-item>
            <el-form-item label="围护结算方式" prop="enclosureMeasureMode">
              <div>
                <span>{{
                  isNotBlank(detail.enclosureMeasureMode) ? enclosureSettlementTypeEnum.VL[detail.enclosureMeasureMode] : ''
                }}</span>
              </div>
            </el-form-item>
          </div>
          <div class="form-row">
            <el-form-item label="运输方式" prop="transportMode">
              <div style="width: 200px">
                <span>{{ isNotBlank(detail.transportMode) ? transportModeEnum.VL[detail.transportMode] : '' }}</span>
              </div>
            </el-form-item>
            <el-form-item label="支付方式" prop="payType">
              <div>
                <span>{{ isNotBlank(detail.payType) ? paymentModeEnum.VL[detail.payType] : '' }}</span>
              </div>
            </el-form-item>
          </div>
          <div class="form-row">
            <el-form-item label="合同含税" prop="isTax">
              <div style="width: 200px">
                <span>{{ isNotBlank(detail.isTax) ? isTaxContractEnum.VL[detail.isTax] : '' }}</span>
              </div>
            </el-form-item>
            <el-form-item label="发票类型" prop="invoiceType">
              <div class="input-underline form-row" style="width: 200px">
                <span>{{ detail.invoiceType ? invoiceTypeEnum.VL[detail.invoiceType] : '' }}</span>
              </div>
            </el-form-item>
            <el-form-item label="税率" prop="businessTaxRate">
                <span>{{ detail.taxRate ? detail.taxRate*100+'%' : '' }}</span>
              </el-form-item>
          </div>
          <div class="form-row">
            <el-form-item label="付款方式描述" prop="payTypeDesc">
              <div class="input-underline" style="width: 550px">
                <span>{{ detail.payTypeDesc }}</span>
              </div>
            </el-form-item>
          </div>
        </div>
        <el-divider><span class="title">技术交底</span></el-divider>
        <enclosure-show :table-data="detail.enclosureInfo" :show-item="showItem"/>
      </div>
    </el-form>
  </div>
</template>

<script setup>
import { ref, defineProps, watch } from 'vue'
import {
  projectTypeEnum,
  businessTypeEnum,
  isTaxContractEnum,
  engineerSettlementTypeEnumN,
  enclosureSettlementTypeEnum,
  transportModeEnum,
  TechnologyTypeEnum
} from '@enum-ms/contract'
import { invoiceTypeEnum, paymentModeEnum } from '@enum-ms/finance'
import { isNotBlank } from '@data-type/index'
import EnclosureShow from '@/views/contract/project-manage/module/enclosure-show'
import { parseTime } from '@/utils/date'

const formRef = ref()
const showItem = ref([])
const form = ref({})

const props = defineProps({
  detail: {
    type: Object,
    default: () => {}
  }
})
const totalArr = [
  TechnologyTypeEnum.SANDWICH_BOARD.V,
  TechnologyTypeEnum.PROFILED_PLATE.V,
  TechnologyTypeEnum.TRUSS_FLOOR_PLATE.V,
  TechnologyTypeEnum.PRESSURE_BEARING_PLATE.V
]
watch(
  () => props.detail,
  (val) => {
    if (isNotBlank(val)) {
      showItem.value = []
      if (val.projectContentList.length > 0) {
        val.projectContentList.forEach(v => {
          if (val.businessType === businessTypeEnum.MACHINING.V) {
            if (v.parentCategory === 'STRUCTURE') {
              if (showItem.value.indexOf(Number(v.no)) < 0) {
                showItem.value.push(TechnologyTypeEnum.STRUCTURE.V)
              }
            } else if (v.parentCategory === 'ENCLOSURE') {
              if (totalArr.indexOf(Number(v.no)) > -1 && showItem.value.indexOf(Number(v.no)) < 0) {
                showItem.value.push(Number(v.no))
              }
            }
          } else {
            if (v.alias) {
              if (v.alias === 'STRUCTURE') {
                if (showItem.value.indexOf(TechnologyTypeEnum.STRUCTURE.V) < 0) {
                  showItem.value.push(TechnologyTypeEnum.STRUCTURE.V)
                }
              } else if (v.alias === 'ENCLOSURE') {
                showItem.value = [...showItem.value, ...totalArr]
              }
            }
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
