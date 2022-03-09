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
                  @change="businessChange"
                  :disabled="detail.monomerQuantity>0"
                />
                <span v-else>{{ detail.businessType? businessTypeEnum.VL[detail.businessType]: '-' }}</span>
              </div>
            </el-form-item>
            <el-form-item label="项目内容" prop="projectContent">
              <div style="width: 320px">
                <el-select
                  v-if="isModify"
                  v-model="form.projectContent"
                  multiple
                  placeholder="项目内容,可多选"
                  class="input-underline"
                  style="width: 320px"
                  @change="getShowItem"
                >
                  <el-option v-for="item in projectContentOption" :key="item.id" :label="item.name" :value="item.id" :disabled="detail.monomerQuantity>0 && originContent.indexOf(item.id)>-1"/>
                </el-select>
                <template v-else>
                  <span v-for="item in detail.projectContentList" :key="item.id">{{ item.name }}&nbsp;</span>
                </template>
              </div>
            </el-form-item>
          </div>
          <div class="form-row">
            <el-form-item label="项目类型" prop="projectType">
              <div style="width: 200px">
                <common-select
                  v-if="isModify"
                  v-model="form.projectType"
                  :options="projectTypeEnumN.ENUM"
                  type="enum"
                  size="small"
                  clearable
                  placeholder="项目类型"
                  style="width: 200px"
                  class="input-underline"
                />
                <span v-else>{{ detail.projectType? projectTypeEnumN.VL[detail.projectType]: '-' }}</span>
              </div>
            </el-form-item>
            <el-form-item label="签约人" prop="signerId">
              <div style="width: 200px">
                <template v-if="isModify">
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
                </template>
                <span v-else>{{ detail.signerName }}</span>
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
                  v-model="form.signingAddress"
                  class="input-underline"
                  placeholder="签约地址"
                  style="width: 400px"
                />
                <span v-else class="detail-break">{{ detail.signingAddress }}</span>
              </div>
            </el-form-item>
          </div>
          <div class="form-row">
            <el-form-item label="工程结算方式" prop="structureMeasureMode">
              <div style="width: 200px">
                <common-radio v-if="isModify" v-model="form.structureMeasureMode" :options="engineerSettlementTypeEnumN.ENUM" type="enum" :disabled="!form.structureMeasureMode"/>
                <span v-else>{{
                  isNotBlank(detail.structureMeasureMode) ? engineerSettlementTypeEnumN.VL[detail.structureMeasureMode] : ''
                }}</span>
              </div>
            </el-form-item>
            <el-form-item label="围护结算方式" prop="enclosureMeasureMode">
              <div>
                <common-radio v-if="isModify" v-model="form.enclosureMeasureMode" :options="enclosureSettlementTypeEnum.ENUM" type="enum" :disabled="!form.enclosureMeasureMode"/>
                <span v-else>{{
                  isNotBlank(detail.enclosureMeasureMode) ? enclosureSettlementTypeEnum.VL[detail.enclosureMeasureMode] : ''
                }}</span>
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
            <el-form-item label="支付方式" prop="payType">
              <div>
                <common-radio v-if="isModify" v-model="form.payType" :options="paymentModeEnum.ENUM" type="enum" />
                <span v-else>{{ isNotBlank(detail.payType) ? paymentModeEnum.VL[detail.payType] : '' }}</span>
              </div>
            </el-form-item>
          </div>
          <div class="form-row">
            <el-form-item label="合同含税" prop="isTax">
              <div style="width: 200px">
                 <el-radio-group v-model="form.isTax" v-if="isModify">
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
                  />
                </template>
                <template v-else>
                  <span>{{ detail.invoiceType ? invoiceTypeEnum.VL[detail.invoiceType] : '' }}</span>
                </template>
              </div>
            </el-form-item>
          </div>
          <div class="form-row">
            <el-form-item label="付款方式描述" prop="payTypeDesc">
              <div class="input-underline" style="width: 550px">
                <el-input
                  v-if="isModify"
                  v-model="form.payTypeDesc"
                  type="textarea"
                  :autosize="{ minRows: 4, maxRows: 4 }"
                  maxlength="200"
                  show-word-limit
                  placeholder="付款方式描述"
                />
                <span v-else>{{ detail.payTypeDesc }}</span>
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
          v-model:visible="enclosureVisible"
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
            <enclosure-form ref="enclosureFormRef" :show-item="showItem" :show-category="showCategory" :init-form="form.enclosureInfo" :default-type="defaultType"/>
          </template>
        </common-drawer>
      </div>
    </el-form>
  </div>
</template>

<script setup>
import { ref, defineProps, watch, defineExpose, nextTick } from 'vue'
import userDeptCascader from '@comp-base/user-dept-cascader.vue'
import branchCompanySelect from '@comp-base/branch-company-select.vue'
import useWatchFormValidate from '@compos/form/use-watch-form-validate'
import { ElRadioGroup } from 'element-plus'
import {
  projectTypeEnumN,
  businessTypeEnum,
  paymentModeEnum,
  invoiceTypeEnum,
  isTaxContractEnum,
  engineerSettlementTypeEnumN,
  enclosureSettlementTypeEnum,
  transportModeEnum,
  TechnologyTypeEnum
} from '@enum-ms/contract'
import { isNotBlank } from '@data-type/index'
import EnclosureShow from '@/views/contract/project-manage/module/enclosure-show'
import EnclosureForm from '@/views/contract/project-manage/module/enclosure-form'
import { getContractBusiness, getContractTechInfo, getContentInfo } from '@/api/contract/project'
import { parseTime } from '@/utils/date'

const formRef = ref()
let projectContent1 = []
let projectContent2 = []
let originConstruct = []
const projectContentOption = ref([])
const showItem = ref([])
const showCategory = ref([])
const enclosureVisible = ref(false)
const enclosureFormRef = ref()

const defaultForm = {
  contractSignBodyId: undefined, // 合同签订主体
  businessType: undefined, // 业务类型
  projectType: undefined, // 项目类型
  projectContent: [], // 项目内容
  projectContentList: [],
  signerId: undefined, // 签约人
  signingDate: undefined, // 签约日期
  signingAddress: undefined, // 签约地址
  structureMeasureMode: engineerSettlementTypeEnumN.THEORY.V, // 结算方式
  enclosureMeasureMode: enclosureSettlementTypeEnum.LENGTH.V, // 围护结算方式
  transportMode: transportModeEnum.HOME_DELIVERY.V, // 运输方式
  payType: paymentModeEnum.PUBLIC_TRANSFER.V, // 付款方式
  isTax: isTaxContractEnum.YES.V, // 是否含税
  invoiceType: invoiceTypeEnum.SPECIAL.V, // 发票类型
  payTypeDesc: undefined, // 付款方式描述
  enclosureInfo: {},
  structureList: [],
  profiledPlateList: [],
  pressureBearingPlateList: [],
  trussFloorPlateList: [],
  sandwichBoardList: []
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
const rules = {
  payTypeDesc: [{ max: 200, message: '不能超过 200 个字符', trigger: 'blur' }],
  businessType: [{ required: true, message: '请选择业务类型', trigger: 'change' }],
  projectType: [{ required: true, message: '请选择项目类型', trigger: 'change' }],
  projectContent: [
    { required: true, validator: validateContent, trigger: 'change' }
  ],
  contractSignBodyId: [
    { required: true, message: '请选择合同签订主体（签订主体可在配置管理-基础配置-分支机构中创建）', trigger: 'change' }
  ]
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
  projectContentOption.value = form.value.businessType === businessTypeEnum.ENUM.INSTALLATION.V ? projectContent2 : projectContent1
  if (detail.value.projectContentList && detail.value.projectContentList.length > 0) {
    detail.value.projectContentList.forEach((v) => {
      form.value.projectContent.push(v.id)
    })
  }
  getShowItem(form.value.projectContent)
  if (formRef.value) {
    nextTick(() => {
      formRef.value.clearValidate()
    })
  }
  useWatchFormValidate(formRef, form)
}

function businessChange() {
  projectContentOption.value = []
  form.value.projectContent = []
  showItem.value = []
  showCategory.value = []
  form.value.structureMeasureMode = undefined
  form.value.enclosureMeasureMode = undefined
  Object.assign(form.value, JSON.parse(JSON.stringify(techForm)))
  if (form.value.businessType) {
    projectContentOption.value = form.value.businessType === businessTypeEnum.ENUM.MACHINING.V ? projectContent1 : projectContent2
  }
}

// function handleAddEnclosure() {
//   if (!form.value.projectContent || form.value.projectContent.length === 0) {
//     ElMessage.error('请先选择项目内容')
//     return
//   }
//   enclosureVisible.value = true
// }

function getShowItem(val) {
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
  const AllInfo = []
  if (val.length > 0) {
    val.map((v) => {
      if (form.value.businessType === businessTypeEnum.ENUM.MACHINING.V) {
        const val = projectContent1.find((k) => k.id === v)
        AllInfo.push(val)
        if (val.alias === 'STRUCTURE') {
          if (showItem.value.indexOf(TechnologyTypeEnum.STRUCTURE.V) < 0) {
            showItem.value.push(TechnologyTypeEnum.STRUCTURE.V)
          }
          showCategory.value.push(val)
        } else {
          if (totalArr.indexOf(Number(val.no)) > -1 && showItem.value.indexOf(Number(val.no)) < 0) {
            showItem.value.push(Number(val.no))
          }
        }
      } else {
        const val = projectContent2.find((k) => k.id === v)
        AllInfo.push(val)
        if (val.alias) {
          if (val.alias === 'STRUCTURE') {
            if (showItem.value.indexOf(TechnologyTypeEnum.STRUCTURE.V) < 0) {
              showItem.value.push(TechnologyTypeEnum.STRUCTURE.V)
              showCategory.value = originConstruct
            }
          } else if (val.alias === 'ENCLOSURE') {
            showItem.value = [...showItem.value, ...totalArr]
          }
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
    form.value.structureMeasureMode = AllInfo.findIndex(v => v.alias === 'STRUCTURE') > -1 ? engineerSettlementTypeEnumN.THEORY.V : undefined
    form.value.enclosureMeasureMode = AllInfo.findIndex(v => v.alias === 'ENCLOSURE') > -1 ? enclosureSettlementTypeEnum.LENGTH.V : undefined
  }
}

function typeChange(val) {
  defaultType.value = val
}

// 围护保存
function enclosureSave() {
  const info = enclosureFormRef.value.tableData
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
  // const loading = ElLoading.service({
  //   target: '#businessContainer',
  //   lock: true,
  //   text: '请稍后，正在加载合同基础信息',
  //   fullscreen: false
  // })
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
    const options = []
    originConstruct = []
    const data1 = await getContentInfo({ businessType: businessTypeEnum.MACHINING.V })
    const data2 = await getContentInfo({ businessType: businessTypeEnum.INSTALLATION.V })
    if (data1 && data1.projectContentVOList.length > 0) {
      data1.projectContentVOList.forEach((v) => {
        if (v.contentList.length > 0) {
          v.contentList.forEach((k) => {
            k.alias = v.type
            options.push(k)
            if (k.alias === 'STRUCTURE') {
              if (originConstruct.indexOf(k) < 0) {
                originConstruct.push(k)
              }
            }
          })
        }
      })
    }
    projectContent1 = options || []
    projectContent2 = data2.projectContentVOList || []
  } catch (error) {
    console.log('error', error)
  } finally {
    detail.value = _detail
    resetForm()
    form.value.projectContent = []
    originContentValue.value = []
    projectContentOption.value = form.value.businessType === businessTypeEnum.ENUM.INSTALLATION.V ? projectContent2 : projectContent1
    if (detail.value.projectContentList && detail.value.projectContentList.length > 0) {
      detail.value.projectContentList.forEach((v) => {
        form.value.projectContent.push(v.id)
        if (detail.value.businessType === businessTypeEnum.MACHINING.V) {
          originContentValue.value.push(v.no)
        } else {
          v.childrenList.forEach(k => {
            originContentValue.value.push(k.no)
          })
        }
      })
    }
    originContent.value = JSON.parse(JSON.stringify(form.value.projectContent))
    detail.value.projectContent = JSON.parse(JSON.stringify(form.value.projectContent))
    getShowItem(form.value.projectContent)
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
