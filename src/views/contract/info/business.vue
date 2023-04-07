<template>
  <div id="businessContainer" class="app-container">
    <el-form ref="formRef" :model="form" :rules="rules" inline size="small" label-position="right" label-width="130px">
      <div>
        <div id="baseInfo">
          <div class="form-row">
            <el-form-item label="合同签订主体" prop="contractSignBodyId">
              <div class="input-underline" style="width: 550px">
                <branch-company-select
                  v-if="isModify"
                  v-model="form.contractSignBodyId"
                  default
                  class="input-underline"
                  placeholder="合同签订主体"
                  style="width: 550px"
                />
                <span v-else>{{ detail.contractSignBodyName }}</span>
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
                <common-select
                  v-if="isModify"
                  v-model="form.businessType"
                  :options="businessTypeEnum.ENUM"
                  type="enum"
                  size="small"
                  clearable
                  placeholder="业务类型"
                  class="input-underline"
                  :disabled="detail.monomerQuantity>0"
                />
                <span v-else>{{ detail.businessType? businessTypeEnum.VL[detail.businessType]: '-' }}</span>
              </div>
            </el-form-item>
            <el-form-item label="项目类型" prop="projectType">
              <template #label>
                项目类型
                <el-tooltip
                  effect="light"
                  :content="`已创建工作计划时不能修改`"
                  placement="top"
                >
                  <i class="el-icon-info" />
                </el-tooltip>
              </template>
              <div style="width: 200px">
                <common-select
                  v-if="isModify"
                  v-model="form.projectType"
                  :options="projectTypeEnum.ENUM"
                  type="enum"
                  size="small"
                  clearable
                  placeholder="项目类型"
                  style="width: 200px"
                  class="input-underline"
                  @change="projectTypeChange"
                  :disabled="detail.monomerQuantity>0"
                />
                <span v-else>{{ detail.projectType? projectTypeEnum.VL[detail.projectType]: '-' }}</span>
              </div>
            </el-form-item>
            <el-form-item label="项目内容" prop="projectContent">
              <div style="width: 320px">
                <el-cascader
                  v-if="isModify"
                  ref="cascaderRef"
                  v-model="form.projectContent"
                  placeholder="项目内容,可多选"
                  :options="projectContentOption"
                  class="input-underline"
                  :props="cascaderProps"
                  :show-all-levels="true"
                  :clearable="true"
                  style="width: 320px"
                  @change="getShowItem"
                  filterable
                />
                <template v-else>
                  <span v-for="item in detail.projectContentList" :key="item.id">{{ item.name }}&nbsp;</span>
                </template>
              </div>
            </el-form-item>
          </div>
          <div class="form-row">
            <el-form-item label="签订日期" prop="signingDate">
              <div style="width: 200px">
                <el-date-picker
                  v-if="isModify"
                  v-model="form.signingDate"
                  type="date"
                  value-format="x"
                  placeholder="选择签订日期"
                  class="input-underline"
                  style="width: 200px"
                />
                <template v-else>
                  <span v-if="detail.signingDate">{{ parseTime(detail.signingDate,'{y}-{m}-{d}') }}</span>
                </template>
              </div>
            </el-form-item>
            <el-form-item label="签约地址" prop="signingAddress">
              <div style="width: 400px">
                <el-input
                  v-if="isModify"
                  v-model.trim="form.signingAddress"
                  class="input-underline"
                  placeholder="签约地址"
                  style="width: 400px"
                />
                <span v-else class="detail-break">{{ detail.signingAddress }}</span>
              </div>
            </el-form-item>
          </div>
          <!-- <div class="form-row">
            <el-form-item label="工程结算方式" prop="structureMeasureMode">
              <div style="width: 200px">
                <common-radio v-if="isModify" v-model="form.structureMeasureMode" :options="engineerSettlementTypeEnumN.ENUM" type="enum" :disabled="!form.structureMeasureMode"/>
                <span v-else>{{
                  isNotBlank(detail.structureMeasureMode) ? engineerSettlementTypeEnumN.VL[detail.structureMeasureMode] : ''
                }}</span>
              </div>
            </el-form-item>
            <el-form-item label="围护结算方式" prop="enclosureMeasureMode" v-if="isModify?(form.measureModeList?.length>0?true:false):(detail.measureModeList?.length>0?true:false)">
              <div>
                <template v-if="isModify">
                  <div v-for="(item,index) in form.measureModeList" :key="index" >
                    <span style="float:left;width:90px;text-align:right;">{{TechnologyTypeAllEnum.VL[item.no]}}：</span><common-radio style="float:left;" v-model="item.measureMode" :options="enclosureSettlementTypeEnum.ENUM" type="enum"/>
                  </div>
                </template>
                <template v-else>
                  <div v-for="(item,index) in detail.measureModeList" :key="index" >
                    <span style="float:left;width:90px;text-align:right;">{{TechnologyTypeAllEnum.VL[item.no]}}：</span>
                    <span style="float:left;">{{enclosureSettlementTypeEnum.VL[item.measureMode]}}</span>
                  </div>
                </template>
              </div>
            </el-form-item>
          </div>
          <div class="form-row">
            <el-form-item label="运输方式" prop="transportMode">
              <div style="width: 200px">
                <common-radio v-if="isModify" v-model="form.transportMode" :options="transportModeEnum.ENUM" type="enum" />
                <span v-else>{{ isNotBlank(detail.transportMode) ? transportModeEnum.VL[detail.transportMode] : '' }}</span>
              </div>
            </el-form-item>
          </div> -->
          <div v-if="isModify">
            <div v-if="form.structureMeasureMode">
              <el-divider><span class="title">结构</span></el-divider>
              <div class="form-row">
                <el-form-item label="结构工程量" prop="quantityWork">
                  <el-input-number
                    v-model="form.quantityWork"
                    :step="1"
                    :min="0"
                    :max="99999999"
                    :precision="0"
                    :controls="false"
                    controls-position="right"
                    class="input-underline"
                    style="width: 200px"
                    placeholder=""
                  />吨
                </el-form-item>
                <el-form-item label="结构类型" prop="structureStatus">
                  <common-select
                    type="enum"
                    size="small"
                    v-model="form.structureStatus"
                    :options="structureTypeEnum.ENUM"
                    class="input-underline"
                    placeholder="结构类型"
                    style="width: 200px"
                  />
                </el-form-item>
              </div>
              <div class="form-row">
                <el-form-item label="结构结算方式" prop="structureMeasureMode">
                  <common-radio v-model="form.structureMeasureMode" :options="engineerSettlementTypeEnumN.ENUM" type="enum" :disabled="!form.structureMeasureMode"/>
                </el-form-item>
                <el-form-item label="结构运输方式" prop="transportMode">
                  <common-radio v-model="form.transportMode" :options="transportModeEnum.ENUM" type="enum" />
                </el-form-item>
              </div>
            </div>
            <div class="form-row" v-if="form.measureModeList && form.measureModeList.length>0">
              <el-divider><span class="title">围护</span></el-divider>
              <el-form-item label="围护运输方式" prop="enclosureTransportMode">
                <common-radio v-model="form.enclosureTransportMode" :options="transportModeEnum.ENUM" type="enum" />
              </el-form-item>
              <el-form-item label="围护结算方式与工程量" prop="measureModeList" v-if="form.measureModeList?.length>0">
                <template v-if="form.measureModeList.length>0">
                  <div v-for="(item,index) in form.measureModeList" :key="index" style="display:flex;">
                    <span style="float:left;width:90px;text-align:right;">{{TechnologyTypeAllEnum.VL[item.no]}}：</span>
                    <common-radio style="float:left;" v-model="item.measureMode" :options="enclosureSettlementTypeEnum.ENUM" type="enum"/>
                    <el-input-number
                    v-model="item.quantityWork"
                    :step="1"
                    :min="0"
                    :max="99999999"
                    :precision="0"
                    :controls="false"
                    controls-position="right"
                    class="input-underline"
                    style="width: 120px;margin-left:10px;"
                    placeholder="工程量"
                  />
                  </div>
                </template>
              </el-form-item>
            </div>
          </div>
          <div v-else>
             <div v-if="detail.structureMeasureMode">
              <el-divider><span class="title">结构</span></el-divider>
              <div class="form-row">
                <el-form-item label="结构工程量" prop="quantityWork">
                  <span>{{detail.quantityWork}}</span>吨
                </el-form-item>
                <el-form-item label="结构类型" prop="structureStatus">
                  <span>{{structureTypeEnum.VL[detail.structureStatus]}}</span>
                </el-form-item>
              </div>
              <div class="form-row">
                <el-form-item label="结构结算方式" prop="structureMeasureMode">
                  <common-radio v-model="detail.structureMeasureMode" :options="engineerSettlementTypeEnumN.ENUM" type="enum" :disabled="true"/>
                </el-form-item>
                <el-form-item label="结构运输方式" prop="transportMode">
                  <common-radio v-model="form.transportMode" :options="transportModeEnum.ENUM" type="enum" :disabled="true"/>
                </el-form-item>
              </div>
            </div>
            <div class="form-row" v-if="detail.measureModeList && detail.measureModeList.length>0">
              <el-divider><span class="title">围护</span></el-divider>
              <el-form-item label="围护运输方式" prop="enclosureTransportMode">
                <common-radio v-model="detail.enclosureTransportMode" :options="transportModeEnum.ENUM" type="enum" :disabled="true"/>
              </el-form-item>
              <el-form-item label="围护结算方式与工程量" prop="measureModeList" v-if="detail.measureModeList?.length>0">
                <template v-if="detail.measureModeList.length>0">
                  <div v-for="(item,index) in detail.measureModeList" :key="index" style="display:flex;">
                    <span style="float:left;width:90px;text-align:right;">{{TechnologyTypeAllEnum.VL[item.no]}}：</span>
                    <common-radio style="float:left;" v-model="item.measureMode" :options="enclosureSettlementTypeEnum.ENUM" type="enum" :disabled="true"/>
                    <span>{{item.quantityWork}}</span>
                  </div>
                </template>
              </el-form-item>
            </div>
          </div>
          <div class="form-row">
            <el-form-item label="支付方式" prop="payType">
              <div>
                <common-radio v-if="isModify" v-model="form.payType" :options="paymentModeEnum.ENUM" type="enum" />
                <span v-else>{{ isNotBlank(detail.payType) ? paymentModeEnum.VL[detail.payType] : '' }}</span>
              </div>
            </el-form-item>
            <el-form-item label="合同含税" prop="isTax">
              <div style="width: 200px">
                 <el-radio-group v-model="form.isTax" v-if="isModify" @change="isTaxChange">
                    <el-radio
                      v-for="item in isTaxContractEnum.ENUM"
                      :key="item.V"
                      :label="item.V"
                    >
                      {{ item.L }}
                    </el-radio>
                  </el-radio-group>
                <span v-else>{{ isNotBlank(detail.isTax) ? isTaxContractEnum.VL[detail.isTax] : '' }}</span>
              </div>
            </el-form-item>
          </div>
          <div class="form-row">
            <el-form-item label="发票类型" prop="invoiceType">
              <div class="input-underline form-row" style="width: 200px">
                <template v-if="isModify">
                  <common-select
                    type="enum"
                    size="small"
                    v-model="form.invoiceType"
                    :options="invoiceTypeEnum.ENUM"
                    :disabled="!form.isTax"
                    class="input-underline"
                    placeholder="选择发票类型"
                    style="width: 200px"
                    @change="invoiceTypeChange"
                  />
                </template>
                <template v-else>
                  <span>{{ detail.invoiceType ? invoiceTypeEnum.VL[detail.invoiceType] : '' }}</span>
                </template>
              </div>
            </el-form-item>
            <el-form-item label="税率" prop="businessTaxRate" v-if="!isModify || (isModify && form.isTax && form.invoiceType !== invoiceTypeEnum.RECEIPT.V)">
              <template v-if="isModify">
                <el-input-number
                  v-model="form.businessTaxRate"
                  :step="1"
                  :min="0"
                  :max="100"
                  :precision="0"
                  :controls="false"
                  controls-position="right"
                  class="input-underline"
                  style="width: 80px"
                  placeholder="0-100"
                  @change="taxChange"
                  :disabled="!form.isTax || form.invoiceType === invoiceTypeEnum.RECEIPT.V"
                />%
              </template>
              <template v-else>
                <span v-if="detail.isTax && detail.invoiceType !== invoiceTypeEnum.RECEIPT.V">{{ detail.businessTaxRate ? detail.businessTaxRate+'%' : '' }}</span>
              </template>
              </el-form-item>
          </div>
          <div class="form-row">
            <el-form-item label="付款方式描述" prop="payTypeDesc">
              <div class="input-underline" style="width: 600px">
                <el-input
                  v-if="isModify"
                  v-model.trim="form.payTypeDesc"
                  type="textarea"
                  :autosize="{ minRows: 4, maxRows: 8 }"
                  maxlength="2000"
                  show-word-limit
                  placeholder="付款方式描述"
                />
                <div v-else class="detail-break" style="max-height:220px;overflow-y:auto;">{{ detail.payTypeDesc }}</div>
              </div>
            </el-form-item>
          </div>
        </div>
        <el-divider><span class="title">技术交底</span></el-divider>
        <div style="text-align: right; margin-right: 20px">
          <common-button
            v-if="isModify"
            style="margin-left: 20px"
            type="success"
            size="small"
            @click="enclosureVisible = true"
            :disabled="!(showItem && showItem.length > 0)"
            >添加</common-button
          >
        </div>
        <enclosure-show :table-data="!isModify ? detail.enclosureInfo : form.enclosureInfo" :show-item="showItem" @clickChange="typeChange"/>
        <!--围护产品数据弹窗  -->
        <common-drawer
          v-model="enclosureVisible"
          :with-header="true"
          :show-close="false"
          :wrapper-closable="false"
          direction="rtl"
          size="80%"
          :before-close="
            () => {
              enclosureVisible = false
            }
          "
        >
          <template #title>
            <span class="line-title">添加技术交底</span>
            <span>
              <common-button size="small" type="primary" @click="enclosureSave">保存</common-button>
              <common-button size="small" type="info" @click="enclosureVisible = false">取消</common-button>
            </span>
          </template>
          <template #content>
            <enclosure-form ref="enclosureFormRef" :show-item="showItem" :show-category="showCategory" :init-form="form.enclosureInfo" :default-type="defaultType" :enclosureVisible="enclosureVisible"/>
          </template>
        </common-drawer>
      </div>
    </el-form>
  </div>
</template>

<script setup>
import { ref, defineProps, watch, defineExpose, nextTick, computed } from 'vue'
// import userDeptCascader from '@comp-base/user-dept-cascader.vue'
import branchCompanySelect from '@comp-base/branch-company-select.vue'
import useWatchFormValidate from '@compos/form/use-watch-form-validate'
import { ElRadioGroup } from 'element-plus'
import {
  projectTypeEnum,
  businessTypeEnum,
  isTaxContractEnum,
  engineerSettlementTypeEnumN,
  enclosureSettlementTypeEnum,
  transportModeEnum,
  TechnologyTypeEnum,
  TechnologyMainTypeEnum,
  TechnologyTypeAllEnum,
  structureTypeEnum
} from '@enum-ms/contract'
import { invoiceTypeEnum, paymentModeEnum } from '@enum-ms/finance'
import { isNotBlank } from '@data-type/index'
import EnclosureShow from '@/views/contract/project-manage/module/enclosure-show'
import EnclosureForm from '@/views/contract/project-manage/module/enclosure-form'
import { getContractBusiness, getContractTechInfo, getContentInfo } from '@/api/contract/project'
import { parseTime } from '@/utils/date'

const formRef = ref()
let machiningData = []
let installData = []
const showItem = ref([])
const showCategory = ref([])
const enclosureVisible = ref(false)
const enclosureFormRef = ref()
const cascaderProps = computed(() => {
  return {
    value: 'id',
    label: 'name',
    children: 'children',
    expandTrigger: 'hover',
    emitPath: false,
    multiple: true,
    checkStrictly: false
  }
})

const defaultForm = {
  contractSignBodyId: undefined, // 合同签订主体
  businessType: undefined, // 业务类型
  projectType: undefined, // 项目类型
  projectContent: [], // 项目内容
  projectContentList: [],
  // signerId: undefined, // 签约人
  signingDate: undefined, // 签约日期
  signingAddress: undefined, // 签约地址
  structureMeasureMode: engineerSettlementTypeEnumN.THEORY.V, // 结算方式
  measureModeList: [],
  transportMode: transportModeEnum.HOME_DELIVERY.V, // 运输方式
  payType: paymentModeEnum.PUBLIC_TRANSFER.V, // 付款方式
  isTax: isTaxContractEnum.YES.V, // 是否含税
  invoiceType: invoiceTypeEnum.SPECIAL.V, // 发票类型
  businessTaxRate: undefined, // 税率
  taxRate: undefined, // 税率
  payTypeDesc: undefined, // 付款方式描述
  enclosureInfo: {},
  structureList: [],
  profiledPlateList: [],
  pressureBearingPlateList: [],
  trussFloorPlateList: [],
  sandwichBoardList: [],
  technologyRemark: undefined, // 技术要求描述
  structureStatus: undefined, // 结构
  enclosureTransportMode: transportModeEnum.HOME_DELIVERY.V // 围护运输方式
}
const techForm = {
  enclosureInfo: {},
  structureList: [],
  profiledPlateList: [],
  pressureBearingPlateList: [],
  trussFloorPlateList: [],
  sandwichBoardList: []
}

const form = ref(JSON.parse(JSON.stringify(defaultForm)))
const detail = ref(JSON.parse(JSON.stringify(defaultForm)))
const originContent = ref()
const originContentValue = ref([])
const defaultType = ref()
const validateContent = (rule, value, callback) => {
  if (value.length <= 0) {
    callback(new Error('请选择项目内容'))
  } else {
    if (detail.value.monomerQuantity > 0) {
      originContent.value.forEach(v => {
        if (form.value.projectContent.indexOf(v) < 0) {
          callback(new Error('项目内容只能增加不能减少'))
        }
      })
      callback()
    } else {
      callback()
    }
  }
}

const validateInvoiceType = (rule, value, callback) => {
  if (form.value.isTax) {
    if (!value) {
      callback(new Error('请选择发票类型'))
    }
    callback()
  }
  callback()
}

const validateTax = (rule, value, callback) => {
  if (form.value.invoiceType && form.value.invoiceType !== invoiceTypeEnum.RECEIPT.V) {
    if (!value) {
      callback(new Error('请填写税率'))
    }
    callback()
  }
  callback()
}

const rules = {
  payTypeDesc: [{ max: 2000, message: '不能超过 2000 个字符', trigger: 'blur' }],
  businessType: [{ required: true, message: '请选择业务类型', trigger: 'change' }],
  projectType: [{ required: true, message: '请选择项目类型', trigger: 'change' }],
  projectContent: [
    { required: true, validator: validateContent, trigger: 'change' }
  ],
  contractSignBodyId: [
    { required: true, message: '请选择合同签订主体（签订主体可在配置管理-基础配置-分支机构中创建）', trigger: 'change' }
  ],
  invoiceType: [{ validator: validateInvoiceType, message: '请选择发票类型', trigger: 'change' }],
  businessTaxRate: [{ validator: validateTax, message: '请输入税率', trigger: 'blur' }]
}

const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined
  },
  isModify: {
    type: Boolean,
    default: false
  }
})

const AllContent = computed(() => {
  if (form.value.businessType) {
    const typeData = form.value.businessType === businessTypeEnum.MACHINING.V ? machiningData : installData
    const arr = []
    if (typeData && typeData[projectTypeEnum.STEEL.V].length > 0) {
      typeData[projectTypeEnum.STEEL.V].map(v => {
        if (v.children && v.children.length > 0) {
          v.children.map(k => {
            arr.push(k)
          })
        }
      })
    }
    return arr
  } else {
    return []
  }
})

const projectContentOption = computed(() => {
  if (form.value.businessType && form.value.projectType) {
    switch (form.value.projectType) {
      case projectTypeEnum.STEEL.V:
        return form.value.businessType === businessTypeEnum.MACHINING.V ? machiningData[projectTypeEnum.STEEL.V] : installData[projectTypeEnum.STEEL.V]
      case projectTypeEnum.BRIDGE.V:
        return form.value.businessType === businessTypeEnum.MACHINING.V ? machiningData[projectTypeEnum.BRIDGE.V] : installData[projectTypeEnum.BRIDGE.V]
      default: return form.value.businessType === businessTypeEnum.MACHINING.V ? machiningData[projectTypeEnum.CARBARN.V] : installData[projectTypeEnum.CARBARN.V]
    }
  } else {
    return []
  }
})

watch(
  () => props.projectId,
  (val) => {
    fetchDetail()
  },
  { deep: true, immediate: true }
)

function resetForm() {
  if (formRef.value) {
    formRef.value.resetFields()
  }
  form.value = JSON.parse(JSON.stringify(detail.value))
  form.value.projectContent = []
  if (detail.value.projectContentList && detail.value.projectContentList.length > 0) {
    detail.value.projectContentList.forEach((v) => {
      form.value.projectContent.push(v.id)
    })
  }
  getShowItem(form.value.projectContent, 'detail')
  if (formRef.value) {
    nextTick(() => {
      formRef.value.clearValidate()
    })
  }
  useWatchFormValidate(formRef, form)
}

function projectTypeChange() {
  form.value.projectContent = []
  showItem.value = []
  showCategory.value = []
  form.value.structureMeasureMode = undefined
  form.value.measureModeList = []
  Object.assign(form.value, JSON.parse(JSON.stringify(techForm)))
}

function isTaxChange(val) {
  if (val !== isTaxContractEnum.YES.V) {
    form.value.invoiceType = undefined
    form.value.businessTaxRate = undefined
    form.value.taxRate = undefined
  }
}

function invoiceTypeChange(val) {
  if (val === invoiceTypeEnum.RECEIPT.V) {
    form.value.businessTaxRate = undefined
    form.value.taxRate = undefined
  }
}

function taxChange() {
  form.value.taxRate = form.value.businessTaxRate ? form.value.businessTaxRate / 100 : undefined
}

function getShowItem(val, type) {
  if (form.value.projectType === projectTypeEnum.BRIDGE.V) {
    form.value.structureMeasureMode = engineerSettlementTypeEnumN.THEORY.V
    showItem.value = []
    showCategory.value = []
    form.value.measureModeList = []
    Object.assign(form.value, JSON.parse(JSON.stringify(techForm)))
    return
  }
  showItem.value = []
  showCategory.value = []
  const allItems = [
    TechnologyTypeEnum.STRUCTURE.V,
    TechnologyTypeEnum.SANDWICH_BOARD.V,
    TechnologyTypeEnum.PROFILED_PLATE.V,
    TechnologyTypeEnum.TRUSS_FLOOR_PLATE.V,
    TechnologyTypeEnum.PRESSURE_BEARING_PLATE.V
  ]
  let totalItems = []
  if (detail.value.monomerQuantity > 0) {
    allItems.forEach(v => {
      if (originContentValue.value && originContentValue.value.indexOf(v) < 0) {
        totalItems.push(v)
      }
    })
  } else {
    totalItems = allItems
  }
  const totalArr = [
    TechnologyTypeEnum.SANDWICH_BOARD.V,
    TechnologyTypeEnum.PROFILED_PLATE.V,
    TechnologyTypeEnum.TRUSS_FLOOR_PLATE.V,
    TechnologyTypeEnum.PRESSURE_BEARING_PLATE.V
  ]
  const enclosureItems = [
    TechnologyTypeAllEnum.SANDWICH_BOARD.V,
    TechnologyTypeAllEnum.PROFILED_PLATE.V,
    TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V,
    TechnologyTypeAllEnum.PRESSURE_BEARING_PLATE.V,
    TechnologyTypeAllEnum.BENDING.V
  ]
  const AllInfo = []
  if (val.length > 0) {
    val.map((v) => {
      const val = AllContent.value.find((k) => k.id === v)
      AllInfo.push(val)
      if (val.categoryType === TechnologyMainTypeEnum.STRUCTURE.V) {
        if (showItem.value.indexOf(TechnologyTypeEnum.STRUCTURE.V) < 0) {
          showItem.value.push(TechnologyTypeEnum.STRUCTURE.V)
        }
        showCategory.value.push(val)
      } else {
        if (totalArr.indexOf(val.no) > -1 && showItem.value.indexOf(val.no) < 0) {
          showItem.value.push(val.no)
        }
      }
    })
    totalItems.forEach(v => {
      if (showItem.value.indexOf(v) < 0) {
        if (isNotBlank(form.value.enclosureInfo)) {
          form.value.enclosureInfo[v] = []
        }
      }
    })
    form.value.structureMeasureMode = AllInfo.findIndex(v => v.categoryType === TechnologyMainTypeEnum.STRUCTURE.V) > -1 ? form.value.structureMeasureMode || engineerSettlementTypeEnumN.THEORY.V : undefined
    if (AllInfo.length > 0) {
      if (type !== 'detail') {
        const modeData = []
        AllInfo.map(v => {
          if (enclosureItems.indexOf(v.no) > -1) {
            if (form.value.measureModeList.findIndex(k => k.no === v.no) < 0) {
              modeData.push({
                measureMode: enclosureSettlementTypeEnum.LENGTH.V,
                no: v.no
              })
            } else {
              modeData.push(form.value.measureModeList.find(k => k.no === v.no))
            }
          }
        })
        form.value.measureModeList = modeData
      }
    } else {
      form.value.measureModeList = []
    }
    if (enclosureFormRef.value) {
      enclosureSave()
    }
  }
}

function typeChange(val) {
  defaultType.value = val
}

// 围护保存
function enclosureSave() {
  const info = enclosureFormRef.value.tableData
  info[TechnologyTypeEnum.STRUCTURE.V] = showItem.value.indexOf(TechnologyTypeEnum.STRUCTURE.V) > -1 ? info[TechnologyTypeEnum.STRUCTURE.V] : []
  info[TechnologyTypeEnum.PROFILED_PLATE.V] = showItem.value.indexOf(TechnologyTypeEnum.PROFILED_PLATE.V) > -1 ? info[TechnologyTypeEnum.PROFILED_PLATE.V] : []
  info[TechnologyTypeEnum.PRESSURE_BEARING_PLATE.V] = showItem.value.indexOf(TechnologyTypeEnum.PRESSURE_BEARING_PLATE.V) > -1 ? info[TechnologyTypeEnum.PRESSURE_BEARING_PLATE.V] : []
  info[TechnologyTypeEnum.TRUSS_FLOOR_PLATE.V] = showItem.value.indexOf(TechnologyTypeEnum.TRUSS_FLOOR_PLATE.V) > -1 ? info[TechnologyTypeEnum.TRUSS_FLOOR_PLATE.V] : []
  info[TechnologyTypeEnum.SANDWICH_BOARD.V] = showItem.value.indexOf(TechnologyTypeEnum.SANDWICH_BOARD.V) > -1 ? info[TechnologyTypeEnum.SANDWICH_BOARD.V] : []
  form.value = {
    ...form.value,
    enclosureInfo: info,
    structureList: info[TechnologyTypeEnum.STRUCTURE.V],
    profiledPlateList: info[TechnologyTypeEnum.PROFILED_PLATE.V],
    pressureBearingPlateList: info[TechnologyTypeEnum.PRESSURE_BEARING_PLATE.V],
    trussFloorPlateList: info[TechnologyTypeEnum.TRUSS_FLOOR_PLATE.V],
    sandwichBoardList: info[TechnologyTypeEnum.SANDWICH_BOARD.V]
  }
  enclosureVisible.value = false
}

async function validateForm() {
  try {
    const valid = await formRef.value.validate()
    return valid
  } catch (error) {
    console.log('error', error)
    return false
  }
}

async function fetchDetail() {
  if (!props.projectId) {
    return
  }
  let _detail = {}
  try {
    const res = await getContractBusiness(props.projectId)
    _detail = JSON.parse(JSON.stringify(res))
    const data = await getContractTechInfo(props.projectId)
    _detail.signingDate = _detail.signingDate ? String(_detail.signingDate) : ''
    _detail.enclosureInfo = {
      [TechnologyTypeEnum.STRUCTURE.V]: data.structureList ? data.structureList : [],
      [TechnologyTypeEnum.PROFILED_PLATE.V]: data.profiledPlateList ? data.profiledPlateList : [],
      [TechnologyTypeEnum.TRUSS_FLOOR_PLATE.V]: data.trussFloorPlateList ? data.trussFloorPlateList : [],
      [TechnologyTypeEnum.PRESSURE_BEARING_PLATE.V]: data.pressureBearingPlateList ? data.pressureBearingPlateList : [],
      [TechnologyTypeEnum.SANDWICH_BOARD.V]: data.sandwichBoardList ? data.sandwichBoardList : []
    }
    _detail.structureList = data.structureList ? data.structureList : []
    _detail.profiledPlateList = data.profiledPlateList ? data.profiledPlateList : []
    _detail.trussFloorPlateList = data.trussFloorPlateList ? data.trussFloorPlateList : []
    _detail.pressureBearingPlateList = data.pressureBearingPlateList ? data.pressureBearingPlateList : []
    _detail.sandwichBoardList = data.sandwichBoardList ? data.sandwichBoardList : []
    machiningData = await getContentInfo({ businessType: businessTypeEnum.MACHINING.V })
    installData = await getContentInfo({ businessType: businessTypeEnum.INSTALLATION.V })
    const dataArr = [machiningData, installData]
    for (let i = 0; i < dataArr.length; i++) {
      if (dataArr[i] && dataArr[i][projectTypeEnum.STEEL.V].length > 0) {
        dataArr[i][projectTypeEnum.STEEL.V].map(v => {
          v.name = v.categoryName
        })
      }
      if (dataArr[i] && dataArr[i][projectTypeEnum.BRIDGE.V].length > 0) {
        dataArr[i][projectTypeEnum.BRIDGE.V].map(v => {
          v.name = v.categoryName
        })
      }
      if (dataArr[i] && dataArr[i][projectTypeEnum.CARBARN.V].length > 0) {
        dataArr[i][projectTypeEnum.CARBARN.V].map(v => {
          v.name = v.categoryName
        })
      }
    }
    _detail.businessTaxRate = _detail.taxRate ? _detail.taxRate * 100 : undefined
  } catch (error) {
    console.log('error', error)
  } finally {
    detail.value = _detail
    resetForm()
    form.value.projectContent = []
    originContentValue.value = []
    if (detail.value.projectContentList && detail.value.projectContentList.length > 0) {
      detail.value.projectContentList.forEach((v) => {
        form.value.projectContent.push(v.id)
        originContentValue.value.push(v.no)
      })
    }
    originContent.value = JSON.parse(JSON.stringify(form.value.projectContent))
    detail.value.projectContent = JSON.parse(JSON.stringify(form.value.projectContent))
    getShowItem(form.value.projectContent, 'detail')
  }
}

defineExpose({
  form,
  validateForm,
  fetchDetail,
  resetForm,
  detail
})
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
