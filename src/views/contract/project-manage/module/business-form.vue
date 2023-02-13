<template>
  <el-form ref="formRef" :model="form" :rules="rules" inline size="small" label-position="right" label-width="110px" class="form-margin">
    <div>
      <div id="baseInfo">
        <div class="form-row">
          <el-form-item label="合同签订主体" prop="contractSignBodyId">
            <branch-company-select
              v-model="form.contractSignBodyId"
              default
              class="input-underline"
              placeholder="合同签订主体"
              style="width: 550px"
            />
          </el-form-item>
        </div>
        <div class="form-row">
          <el-form-item label="业务类型" prop="businessType">
            <common-select
              v-model="form.businessType"
              :options="businessTypeEnum.ENUM"
              type="enum"
              size="small"
              clearable
              placeholder="业务类型"
              class="input-underline"
            />
          </el-form-item>
          <el-form-item label="项目类型" prop="projectType">
            <common-select
              v-model="form.projectType"
              :options="projectTypeEnum.ENUM"
              type="enum"
              size="small"
              clearable
              placeholder="项目类型"
              style="width: 200px"
              class="input-underline"
              @change="projectTypeChange"
            />
          </el-form-item>
          <el-form-item label="项目内容" prop="projectContent">
            <el-cascader
              ref="cascaderRef"
              v-model="form.projectContent"
              placeholder="项目内容,可多选"
              :options="projectContentOption"
              class="input-underline"
              :props="cascaderProps"
              :show-all-levels="true"
              :clearable="true"
              style="width: 320px"
              filterable
              @change="getShowItem"
            />
          </el-form-item>
        </div>
        <div class="form-row">
          <el-form-item label="签约人" prop="signerId">
            <user-dept-cascader
              v-model="form.signerId"
              filterable
              :collapse-tags="false"
              clearable
              show-all-levels
              class="input-underline"
              style="width: 200px"
              placeholder="签约人"
            />
          </el-form-item>
          <el-form-item label="签订日期" prop="signingDate">
            <el-date-picker
              v-model="form.signingDate"
              type="date"
              value-format="x"
              placeholder="选择签订日期"
              class="input-underline"
              style="width: 200px"
            />
          </el-form-item>
          <el-form-item label="签约地址" prop="signingAddress">
            <el-input v-model.trim="form.signingAddress" class="input-underline" placeholder="签约地址" style="width: 400px" maxlength="200"/>
          </el-form-item>
        </div>
        <div class="form-row">
          <el-form-item label="工程结算方式" prop="structureMeasureMode">
            <common-radio v-model="form.structureMeasureMode" :options="engineerSettlementTypeEnumN.ENUM" type="enum" :disabled="!form.structureMeasureMode"/>
          </el-form-item>
          <el-form-item label="围护结算方式" prop="enclosureMeasureMode" v-if="form.measureModeList?.length>0">
            <template v-if="form.measureModeList.length>0">
              <div v-for="(item,index) in form.measureModeList" :key="index" >
                <span style="float:left;width:90px;text-align:right;">{{TechnologyTypeAllEnum.VL[item.no]}}：</span><common-radio style="float:left;" v-model="item.measureMode" :options="enclosureSettlementTypeEnum.ENUM" type="enum"/>
              </div>
            </template>
          </el-form-item>
        </div>
        <div class="form-row">
          <el-form-item label="运输方式" prop="transportMode">
            <common-radio v-model="form.transportMode" :options="transportModeEnum.ENUM" type="enum" />
          </el-form-item>
          <el-form-item label="支付方式" prop="payType">
            <common-radio v-model="form.payType" :options="paymentModeEnum.ENUM" type="enum" />
          </el-form-item>
        </div>
        <div class="form-row">
          <el-form-item label="合同含税" prop="isTax">
            <el-radio-group v-model="form.isTax" @change="isTaxChange">
              <el-radio
                v-for="item in isTaxContractEnum.ENUM"
                :key="item.V"
                :label="item.V"
              >
                {{ item.L }}
              </el-radio>
            </el-radio-group>
          </el-form-item>
          <el-form-item label="发票类型" prop="invoiceType">
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
          </el-form-item>
          <el-form-item label="税率" prop="businessTaxRate" v-if="form.isTax && form.invoiceType !== invoiceTypeEnum.RECEIPT.V">
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
              :disabled="!form.isTax || form.invoiceType === invoiceTypeEnum.RECEIPT.V"
            />%
          </el-form-item>
        </div>
        <div class="form-row">
          <el-form-item label="支付方式描述" prop="payTypeDesc">
            <el-input
              v-model.trim="form.payTypeDesc"
              type="textarea"
              :autosize="{ minRows: 4, maxRows: 8 }"
              class="input-underline"
              maxlength="2000"
              show-word-limit
              style="width: 550px"
              placeholder="付款方式描述"
            />
          </el-form-item>
        </div>
      </div>
      <el-divider v-if="form.projectType!==projectTypeEnum.BRIDGE.V"><span class="title">技术交底</span></el-divider>
      <div style="text-align: right; margin-right: 20px" v-if="form.projectType!==projectTypeEnum.BRIDGE.V">
        <common-button style="margin-left: 20px" type="success" size="small" :disabled="!(showItem && showItem.length > 0)" @click="handleAddEnclosure">添加</common-button>
      </div>
      <enclosure-show :table-data="form.enclosureInfo" :show-item="showItem" @clickChange="typeChange"  v-if="showItem && showItem.length > 0"/>
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
            <common-button v-if="showItem && showItem.length > 0" size="small" type="primary" @click="enclosureSave">保存</common-button>
            <common-button size="small" type="info" @click="enclosureVisible = false">取消</common-button>
          </span>
        </template>
        <template #content>
          <enclosure-form ref="enclosureFormRef" :show-item="showItem" :show-category="showCategory" :default-type="defaultType" :init-form="form.enclosureInfo" :enclosureVisible="enclosureVisible"/>
        </template>
      </common-drawer>
    </div>
  </el-form>
</template>

<script setup>
import { ref, defineProps, watch, defineExpose, nextTick, computed } from 'vue'
import userDeptCascader from '@comp-base/user-dept-cascader.vue'
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
  TechnologyTypeAllEnum
} from '@enum-ms/contract'
import { invoiceTypeEnum, paymentModeEnum } from '@enum-ms/finance'
import { getContentInfo } from '@/api/contract/project'
import { ElMessage } from 'element-plus'
import enclosureForm from './enclosure-form'
import enclosureShow from './enclosure-show'
import { isNotBlank } from '@/utils/data-type'

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

const props = defineProps({
  formData: {
    type: Object,
    default: () => {}
  }
})
const defaultForm = {
  contractSignBodyId: undefined, // 合同签订主体
  businessType: undefined, // 业务类型
  projectType: undefined, // 项目类型
  projectContent: [], // 项目内容
  signerId: undefined, // 签约人
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
  structureSaveRequestVOS: [],
  profiledPlateSaveRequestVOS: [],
  pressureBearingPlateSaveVOS: [],
  trussFloorPlateSaveRequestVOS: [],
  sandwichBoardSaveRequestVOS: []
}

const techForm = {
  enclosureInfo: {},
  structureSaveRequestVOS: [],
  profiledPlateSaveRequestVOS: [],
  pressureBearingPlateSaveVOS: [],
  trussFloorPlateSaveRequestVOS: [],
  sandwichBoardSaveRequestVOS: []
}

const form = ref(JSON.parse(JSON.stringify(defaultForm)))

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
  projectContent: [{ required: true, message: '请输入项目内容', trigger: 'change' }],
  contractSignBodyId: [
    { required: true, message: '请选择合同签订主体（签订主体可在配置管理-基础配置-分支机构中创建）', trigger: 'change' }
  ],
  invoiceType: [{ validator: validateInvoiceType, message: '请选择发票类型', trigger: 'change' }],
  businessTaxRate: [{ validator: validateTax, message: '请输入税率', trigger: 'blur' }]
}
const defaultType = ref()

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
  () => props.formData,
  (val) => {
    resetForm(val)
    contentInfo()
  },
  { deep: true, immediate: true }
)

function resetForm(data) {
  // 清除表单信息
  if (formRef.value) {
    formRef.value.resetFields()
  }
  let formVal
  if (data && Object.keys(data).length > 0) {
    formVal = JSON.parse(JSON.stringify(data))
  } else {
    formVal = JSON.parse(JSON.stringify(defaultForm))
    Object.assign(form.value, JSON.parse(JSON.stringify(techForm)))
    defaultType.value = undefined
  }
  form.value = JSON.parse(JSON.stringify(formVal))
  if (formRef.value) {
    nextTick(() => {
      formRef.value.clearValidate()
    })
  }
  useWatchFormValidate(formRef, form)
}

contentInfo()

async function contentInfo() {
  try {
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
  } catch (error) {
    console.log(error)
  }
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

function handleAddEnclosure() {
  if (!form.value.projectContent || form.value.projectContent.length === 0) {
    ElMessage.error('请先选择项目内容')
    return
  }
  enclosureVisible.value = true
}

function getShowItem(val) {
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
  const totalItems = [
    TechnologyTypeEnum.STRUCTURE.V,
    TechnologyTypeEnum.SANDWICH_BOARD.V,
    TechnologyTypeEnum.PROFILED_PLATE.V,
    TechnologyTypeEnum.TRUSS_FLOOR_PLATE.V,
    TechnologyTypeEnum.PRESSURE_BEARING_PLATE.V
  ]
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
    structureSaveRequestVOS: info[TechnologyTypeEnum.STRUCTURE.V],
    profiledPlateSaveRequestVOS: info[TechnologyTypeEnum.PROFILED_PLATE.V],
    pressureBearingPlateSaveVOS: info[TechnologyTypeEnum.PRESSURE_BEARING_PLATE.V],
    trussFloorPlateSaveRequestVOS: info[TechnologyTypeEnum.TRUSS_FLOOR_PLATE.V],
    sandwichBoardSaveRequestVOS: info[TechnologyTypeEnum.SANDWICH_BOARD.V]
  }
  enclosureVisible.value = false
}

async function validateForm() {
  try {
    const valid = await formRef.value.validate()
    if (valid) {
      form.value.taxRate = form.value.businessTaxRate ? form.value.businessTaxRate / 100 : undefined
      Object.assign(props.formData, JSON.parse(JSON.stringify(form.value)))
    }
    return valid
  } catch (error) {
    console.log('error', error)
    return false
  }
}

defineExpose({
  validateForm
})
</script>

<style lang="scss" scoped>
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
.form-row {
  width: 100%;
}
.form-margin{
  ::v-deep(.el-form-item){
    margin-right:30px;
  }
}
</style>
