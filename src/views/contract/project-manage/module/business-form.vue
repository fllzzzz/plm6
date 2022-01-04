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
              @change="businessChange"
            />
          </el-form-item>
          <el-form-item label="项目内容" prop="projectContent">
            <el-select
              v-model="form.projectContent"
              multiple
              placeholder="项目内容,可多选"
              class="input-underline"
              style="width: 320px"
              @change="getShowItem"
            >
              <el-option v-for="item in projectContentOption" :key="item.id" :label="item.name" :value="item.id" />
            </el-select>
          </el-form-item>
        </div>
        <div class="form-row">
          <el-form-item label="项目类型" prop="projectType">
            <common-select
              v-model="form.projectType"
              :options="projectTypeEnumN.ENUM"
              type="enum"
              size="small"
              clearable
              placeholder="项目类型"
              style="width: 200px"
              class="input-underline"
            />
          </el-form-item>
          <el-form-item label="签约人" prop="singerId">
            <user-dept-cascader
              v-model="form.singerId"
              filterable
              :collapse-tags="false"
              clearable
              show-all-levels
              class="input-underline"
              style="width: 320px"
              placeholder="签约人"
            />
          </el-form-item>
        </div>
        <div class="form-row">
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
            <el-input v-model="form.signingAddress" class="input-underline" placeholder="签约地址" style="width: 400px" maxlength="200"/>
          </el-form-item>
        </div>
        <div class="form-row">
          <el-form-item label="工程结算方式" prop="structureMeasureMode">
            <common-radio v-model="form.structureMeasureMode" :options="engineerSettlementTypeEnumN.ENUM" type="enum" />
          </el-form-item>
          <el-form-item label="围护结算方式" prop="enclosureMeasureMode">
            <common-radio v-model="form.enclosureMeasureMode" :options="enclosureSettlementTypeEnum.ENUM" type="enum" />
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
            <el-radio-group v-model="form.isTax">
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
            />
            <!--            <template v-if="form.invoiceType === invoiceTypeEnum.SPECIAL_INVOICE.V">-->
            <!--              <el-input-number-->
            <!--                v-model.number="form.taxRate"-->
            <!--                :min="0"-->
            <!--                :max="100"-->
            <!--                :step="1"-->
            <!--                :precision="1"-->
            <!--                placeholder="税率"-->
            <!--                size="small"-->
            <!--                :controls="false"-->
            <!--                style="width: 70px;"-->
            <!--                class="input-underline"-->
            <!--              />%-->
            <!--            </template>-->
          </el-form-item>
        </div>
        <div class="form-row">
          <el-form-item label="支付方式描述" prop="payTypeDesc">
            <el-input
              v-model="form.payTypeDesc"
              type="textarea"
              :autosize="{ minRows: 4, maxRows: 4 }"
              class="input-underline"
              maxlength="200"
              show-word-limit
              style="width: 550px"
              placeholder="付款方式描述"
            />
          </el-form-item>
        </div>
      </div>
      <el-divider><span class="title">技术交底</span></el-divider>
      <div style="text-align: right; margin-right: 20px">
        <common-button style="margin-left: 20px" type="success" size="small" @click="handleAddEnclosure">添加</common-button>
      </div>
      <enclosure-show :table-data="form.enclosureInfo" :show-item="showItem" />
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
            <common-button v-if="showItem && showItem.length > 0" size="small" type="primary" @click="enclosureSave">保存</common-button>
            <common-button size="small" type="info" @click="enclosureVisible = false">取消</common-button>
          </span>
        </template>
        <template #content>
          <enclosure-form ref="enclosureFormRef" :show-item="showItem" :show-category="showCategory" />
        </template>
      </common-drawer>
    </div>
  </el-form>
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
import { getContentInfo } from '@/api/contract/project'
import { ElMessage } from 'element-plus'
import enclosureForm from './enclosure-form'
import enclosureShow from './enclosure-show'

const formRef = ref()
let projectContent1 = []
let projectContent2 = []
const originConstruct = []
const projectContentOption = ref([])
const showItem = ref([])
const showCategory = ref([])
const enclosureVisible = ref(false)
const enclosureFormRef = ref()
const props = defineProps({
  formData: {
    type: Object,
    default: () => {}
  }
  // projectType: {
  //   type: Number,
  //   default: undefined
  // }
})
const defaultForm = {
  contractSignBodyId: undefined, // 合同签订主体
  businessType: undefined, // 业务类型
  projectType: undefined, // 项目类型
  projectContent: [], // 项目内容
  singerId: undefined, // 签约人
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
const rules = {
  payTypeDesc: [{ max: 200, message: '不能超过 200 个字符', trigger: 'blur' }],
  businessType: [{ required: true, message: '请选择业务类型', trigger: 'change' }],
  projectType: [{ required: true, message: '请选择项目类型', trigger: 'change' }],
  projectContent: [{ required: true, message: '请输入项目内容', trigger: 'change' }],
  contractSignBodyId: [
    { required: true, message: '请选择合同签订主体（签订主体可在配置管理-基础配置-分支机构中创建）', trigger: 'change' }
  ]
}

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
    const options = []
    const data1 = await getContentInfo({ businessType: businessTypeEnum.ENUM.MACHINING.V })
    const data2 = await getContentInfo({ businessType: businessTypeEnum.ENUM.INSTALLATION.V })
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
    console.log(error)
  }
}

function businessChange() {
  projectContentOption.value = []
  form.value.projectContent = []
  showItem.value = []
  showCategory.value = []
  Object.assign(form.value, JSON.parse(JSON.stringify(techForm)))
  if (form.value.businessType) {
    projectContentOption.value = form.value.businessType === businessTypeEnum.ENUM.MACHINING.V ? projectContent1 : projectContent2
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
  showItem.value = []
  showCategory.value = []
  const totalArr = [
    TechnologyTypeEnum.SANDWICH_BOARD.V,
    TechnologyTypeEnum.PROFILED_PLATE.V,
    TechnologyTypeEnum.TRUSS_FLOOR_PLATE.V,
    TechnologyTypeEnum.PRESSURE_BEARING_PLATE.V
  ]
  if (val.length > 0) {
    val.map((v) => {
      if (form.value.businessType === businessTypeEnum.ENUM.MACHINING.V) {
        const val = projectContent1.find((k) => k.id === v)
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
  }
}
// 围护保存
function enclosureSave() {
  const info = enclosureFormRef.value.tableData
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
