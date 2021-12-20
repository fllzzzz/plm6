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
                <span v-else>{{ detail.signingMainBodyName }}</span>
              </div>
            </el-form-item>
          </div>
          <div class="form-row">
            <el-form-item label="业务类型" prop="businessType">
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
                />
                <span v-else>{{ detail.businessTypeDesc }}</span>
              </div>
            </el-form-item>
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
                <span v-else>{{ detail.projectTypeDesc }}</span>
              </div>
            </el-form-item>
            <el-form-item label="项目内容" prop="content">
              <div style="width: 200px">
                <el-select
                  v-if="isModify"
                  v-model="form.projectContent"
                  multiple
                  placeholder="项目内容,可多选"
                  class="input-underline"
                  style="width: 320px"
                  @change="getshowItem"
                >
                  <el-option v-for="item in projectContentOption" :key="item.id" :label="item.name" :value="item.id" />
                </el-select>
                <template v-else>
                  <span v-for="item in detail.projectContentList" :key="item.id">{{ item.name }}&nbsp;</span>
                </template>
              </div>
            </el-form-item>
          </div>
          <div class="form-row">
            <el-form-item label="签约人" prop="singerId">
              <div style="width: 200px">
                <template v-if="isModify">
                  <user-dept-cascader
                    v-model="form.singerId"
                    filterable
                    :collapse-tags="false"
                    clearable
                    show-all-levels
                    class="input-underline"
                    style="width: 200px"
                    placeholder="签约人"
                  />
                </template>
                <span v-else>{{ detail.singerName }}</span>
              </div>
            </el-form-item>
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
                  <span v-parse-time="'{y}-{m}-{d}'">{{ detail.signingDate }}</span>
                </template>
              </div>
            </el-form-item>
            <el-form-item label="签约地址" prop="signingAddress">
              <div style="width: 200px">
                <el-input
                  v-if="isModify"
                  v-model="form.signingAddress"
                  class="input-underline"
                  placeholder="签约地址"
                  style="width: 200px"
                />
                <span v-else>{{ detail.signingAddress }}</span>
              </div>
            </el-form-item>
          </div>
          <div class="form-row">
            <el-form-item label="工程结算方式" prop="structureMeasureMode">
              <common-radio v-if="isModify" v-model="form.structureMeasureMode" :options="engineerSettlementTypeEnumN.ENUM" type="enum" />
              <span v-else>{{
                isNotBlank(detail.structureMeasureMode) ? engineerSettlementTypeEnumN.VL[detail.structureMeasureMode] : ''
              }}</span>
            </el-form-item>
            <el-form-item label="围护结算方式" prop="enclosureMeasureMode">
              <common-radio v-if="isModify" v-model="form.enclosureMeasureMode" :options="enclosureSettlementTypeEnum.ENUM" type="enum" />
              <span v-else>{{
                isNotBlank(detail.enclosureMeasureMode) ? enclosureSettlementTypeEnum.VL[detail.enclosureMeasureMode] : ''
              }}</span>
            </el-form-item>
          </div>
          <div class="form-row">
            <el-form-item label="运输方式" prop="transportMode">
              <common-radio v-if="isModify" v-model="form.transportMode" :options="transportModeEnum.ENUM" type="enum" />
              <span v-else>{{ isNotBlank(detail.transportMode) ? transportModeEnum.VL[detail.transportMode] : '' }}</span>
            </el-form-item>
            <el-form-item label="支付方式" prop="payType">
              <common-radio v-if="isModify" v-model="form.payType" :options="paymentModeEnum.ENUM" type="enum" />
              <span v-else>{{ isNotBlank(detail.payType) ? paymentModeEnum.VL[detail.payType] : '' }}</span>
            </el-form-item>
          </div>
          <div class="form-row">
            <el-form-item label="合同含税" prop="isTaxInclusive">
              <div style="width: 200px">
                <common-radio v-if="isModify" v-model="form.isTaxInclusive" :options="isTaxEnum.ENUM" type="enum" />
                <span v-else>{{ detail.isTaxInclusive ? isTaxEnum.VL[detail.isTaxInclusive] : '否' }}</span>
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
                    :disabled="!form.isTaxInclusive"
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
            <el-form-item label="付款方式描述" prop="paymentDescription">
              <div class="input-underline" style="width: 550px">
                <el-input
                  v-if="isModify"
                  v-model="form.paymentDescription"
                  type="textarea"
                  :autosize="{ minRows: 4, maxRows: 4 }"
                  placeholder="付款方式描述"
                />
                <span v-else>{{ detail.paymentDescription }}</span>
              </div>
            </el-form-item>
          </div>
        </div>
        <el-divider><span class="title">技术交底</span></el-divider>
        <div style="text-align: right; margin-right: 20px">
          <common-button v-if="isModify" style="margin-left: 20px" type="success" size="small" @click="enclosureVisible = true"
            >添加</common-button
          >
        </div>
        <enclosure-show :table-data="!isModify ? detail.enclosureInfo : form.enclosureInfo" :show-item="showItem" />
        <!--围护产品数据弹窗  -->
        <common-drawer
          :visible.sync="enclosureVisible"
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
            <enclosure-form ref="enclosureFormRef" :show-item="showItem" :show-category="showCategory" :init-form="form.enclosureInfo" />
          </template>
        </common-drawer>
      </div>
    </el-form>
  </div>
</template>

<script setup>
import { ref, defineProps, watch, computed, defineExpose } from 'vue'
import userDeptCascader from '@comp-base/user-dept-cascader.vue'
import branchCompanySelect from '@comp-base/branch-company-select.vue'
import useWatchFormValidate from '@compos/form/use-watch-form-validate'
import {
  projectTypeEnumN,
  businessTypeEnum,
  paymentModeEnum,
  invoiceTypeEnum,
  isTaxEnum,
  engineerSettlementTypeEnumN,
  enclosureSettlementTypeEnum,
  transportModeEnum,
  TechnologyTypeEnum,
} from '@enum-ms/contract'
import { ElMessage, ElLoading } from 'element-plus'
import { isNotBlank } from '@data-type/index'
import EnclosureShow from '@/views/contract/project-manage/module/enclosure-show'
import EnclosureForm from '@/views/contract/project-manage/module/enclosure-form'
import { getContractBusiness, getContractTechInfo, getContentInfo } from '@/api/contract/project'

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
  singerId: undefined, // 签约人
  signingDate: undefined, // 签约日期
  signingAddress: undefined, // 签约地址
  structureMeasureMode: engineerSettlementTypeEnumN.THEORY.V, // 结算方式
  enclosureMeasureMode: enclosureSettlementTypeEnum.LENGTH.V, // 围护结算方式
  transportMode: transportModeEnum.HOME_DELIVERY.V, // 运输方式
  payType: paymentModeEnum.PUBLIC_TRANSFER.V, // 付款方式
  isTaxInclusive: true, // 是否含税
  invoiceType: invoiceTypeEnum.SPECIAL.V, // 发票类型
  paymentDescription: undefined, // 付款方式描述
  enclosureInfo: {},
  structureSaveRequestVOS: [],
  profiledPlateSaveRequestVOS: [],
  pressureBearingPlateSaveVOS: [],
  trussFloorPlateSaveRequestVOS: [],
  sandwichBoardSaveRequestVOS: [],
}
const techForm = {
  enclosureInfo: {},
  structureSaveRequestVOS: [],
  profiledPlateSaveRequestVOS: [],
  pressureBearingPlateSaveVOS: [],
  trussFloorPlateSaveRequestVOS: [],
  sandwichBoardSaveRequestVOS: [],
}

const form = ref(JSON.parse(JSON.stringify(defaultForm)))
const detail = ref(JSON.parse(JSON.stringify(defaultForm)))

const rules = {
  paymentDescription: [{ max: 2000, message: '不能超过 2000 个字符', trigger: 'blur' }],
  businessType: [{ required: true, message: '请选择业务类型', trigger: 'change' }],
  projectType: [{ required: true, message: '请选择项目类型', trigger: 'change' }],
  projectContent: [{ required: true, message: '请输入项目内容', trigger: 'change' }],
  contractSignBodyId: [
    { required: true, message: '请选择合同签订主体（签订主体可在配置管理-基础配置-分支机构中创建）', trigger: 'change' },
  ],
}

const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined,
  },
  isModify: {
    type: Boolean,
    default: false,
  },
})

watch(
  () => props.projectId,
  (val) => {
    fetchDetail()
  },
  { deep: true, immediate: true }
)

function resetForm() {
  // 清除表单信息
  if (formRef.value) {
    formRef.value.resetFields()
  }
  form.value = JSON.parse(JSON.stringify(detail.value))
  useWatchFormValidate(formRef, form)
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

function getshowItem(val) {
  showItem.value = []
  showCategory.value = []
  const totalArr = [
    TechnologyTypeEnum.ENUM.SANDWICH_BOARD.V,
    TechnologyTypeEnum.ENUM.PROFILEDPLATE.V,
    TechnologyTypeEnum.ENUM.TRUSSFLOORPLATE.V,
    TechnologyTypeEnum.ENUM.PRESSUREBEARINGPLATE.V,
  ]
  if (val.length > 0) {
    val.map((v) => {
      if (form.value.businessType === businessTypeEnum.ENUM.MACHINING.V) {
        const val = projectContent1.find((k) => k.id === v)
        if (val.alias === 'STRUCTURE') {
          if (showItem.value.indexOf(TechnologyTypeEnum.ENUM.STRUCTURE.V) < 0) {
            showItem.value.push(TechnologyTypeEnum.ENUM.STRUCTURE.V)
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
            if (showItem.value.indexOf(TechnologyTypeEnum.ENUM.STRUCTURE.V) < 0) {
              showItem.value.push(TechnologyTypeEnum.ENUM.STRUCTURE.V)
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
    structureSaveRequestVOS: info[TechnologyTypeEnum.ENUM.STRUCTURE.V],
    profiledPlateSaveRequestVOS: info[TechnologyTypeEnum.ENUM.PROFILEDPLATE.V],
    pressureBearingPlateSaveVOS: info[TechnologyTypeEnum.ENUM.PRESSUREBEARINGPLATE.V],
    trussFloorPlateSaveRequestVOS: info[TechnologyTypeEnum.ENUM.TRUSSFLOORPLATE.V],
    sandwichBoardSaveRequestVOS: info[TechnologyTypeEnum.ENUM.SANDWICH_BOARD.V],
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
    if (!res.enclosureMeasureMode) {
      res.enclosureMeasureMode = enclosureSettlementTypeEnum.LENGTH.V
    }
    _detail = JSON.parse(JSON.stringify(res))
    const data = await getContractTechInfo(props.projectId)
    _detail.enclosureInfo = {
      [TechnologyTypeEnum.ENUM.STRUCTURE.V]: data.structureList ? data.structureList : [],
      [TechnologyTypeEnum.ENUM.PROFILEDPLATE.V]: data.profiledPlateList ? data.profiledPlateList : [],
      [TechnologyTypeEnum.ENUM.TRUSSFLOORPLATE.V]: data.trussFloorPlateList ? data.trussFloorPlateList : [],
      [TechnologyTypeEnum.ENUM.PRESSUREBEARINGPLATE.V]: data.pressureBearingPlateList ? data.pressureBearingPlateList : [],
      [TechnologyTypeEnum.ENUM.SANDWICH_BOARD.V]: data.sandwichBoardList ? data.sandwichBoardList : [],
    }
    const options = []
    originConstruct = []
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
    console.log('error', error)
  } finally {
    detail.value = _detail
    resetForm()
    form.value.projectContent = []
    projectContentOption.value = form.value.businessType === businessTypeEnum.ENUM.INSTALLATION.V ? projectContent2 : projectContent1
    if (detail.value.projectContentList && detail.value.projectContentList.length > 0) {
      detail.value.projectContentList.forEach((v) => {
        form.value.projectContent.push(v.id)
      })
    }
    getshowItem(form.value.projectContent)
    // loading.close()
  }
}

defineExpose({
  form,
  validateForm,
  fetchDetail,
  resetForm,
})
// import { mapGetters } from 'vuex'
// import { getBusinessInfo as getDetail, downloadSettlementAttachments, getTechInfo } from '@/api/contract/info'
// import branchCompanySelect from '@/views/components/base/branch-company-select'
// // import UploadList from '@/components/FileUpload/UploadList'
// // import ExcelUploadResolve from '@/components/FileUpload/ExcelUploadResolve'
// import { debounce } from '@/utils'
// import enumOperate, { fileClassifyEnum, payTypeEnum, invoiceTypeEnum, engineerSettlementTypeEnum, transportModeEnum, enclosureSettlementTypeEnum, projectTypeEnumN, businessTypeEnum, projectContent, projectContentME, TechnologyTypeEnum as enclosureTypeEnum } from '@/utils/enum/index'
// import { tableValidate } from '@/utils/other'
// import checkPermission from '@/utils/system/check-permission'
// import enclosureShow from '@/views/contract/project/module/enclosure-show'
// import enclosureForm from '@/views/contract/project/module//enclosure-form'
// import userDeptCascader from '@/views/components/base/user-dept-cascader'
// import { getContentInfo } from '@/api/contract/project'

// // crud交由presenter持有
// const permission = {
//   get: ['contract:businessGet', 'contract:detail', 'collectionListContractInfo:detail'],
//   edit: ['contract:businessEdit'],
//   downloadSettlementAttachments: ['contract:downloadSettlementAttachments', 'contract:detail', 'collectionListContractInfo:detail']
// }

// const projectTypeEnumV = enumOperate.getVal(projectTypeEnumN)
// const payTypeEnumV = enumOperate.getVal(payTypeEnum)
// const invoiceTypeEnumV = enumOperate.getVal(invoiceTypeEnum)
// const engineerSettlementTypeEnumV = enumOperate.getVal(engineerSettlementTypeEnum)
// const transportModeEnumV = enumOperate.getVal(transportModeEnum)
// const enclosureSettlementTypeEnumV = enumOperate.getVal(enclosureSettlementTypeEnum)
// const businessTypeEnumV = enumOperate.getVal(businessTypeEnum)
// const projectContentV = enumOperate.getVal(projectContent)
// const projectContentMEV = enumOperate.getVal(projectContentME)

// const defaultForm = {
//   contractSignBodyId: undefined, // 合同签订主体
//   structureMeasureMode: engineerSettlementTypeEnum.THEORY.V, // 工程结算方式
//   enclosureMeasureMode: enclosureSettlementTypeEnum.LENGTH.V, // 围护结算方式
//   // otherSettlement: undefined, // 其他结算方式
//   transportMode: transportModeEnum.HOME_DELIVERY.V, // 运输方式
//   payType: payTypeEnum.PUBLIC_TRANSFER.V, // 收款方式
//   isTaxInclusive: true, // 是否含税
//   invoiceType: invoiceTypeEnum.SPECIAL_INVOICE.V, // 发票类型
//   paymentDescription: undefined, // 付款方式描述
//   finalAmount: undefined, // 最终结算金额
//   settlementAttachments: [], // 结算附件
//   businessList: [] // 商务信息列表
// }
// const excelField = {
//   productCategory: '__EMPTY', // 产品种类
//   engineeringMete: '__EMPTY_1', // 工程量
//   unit: '__EMPTY_2', // 单位
//   unitPrice: '__EMPTY_3', // 单价
//   remark: '__EMPTY_5' // 备注
// }

// export default {
//   name: 'ContractBusinessInfo',
//   components: { branchCompanySelect, enclosureShow, userDeptCascader, enclosureForm },
//   props: {
//     projectId: {
//       type: [Number, String],
//       default: undefined
//     },
//     showEdit: {
//       type: Boolean,
//       default: false
//     }
//   },
//   data() {
//     return {
//       permission,
//       fileClassifyEnum,
//       payTypeEnum,
//       payTypeEnumV,
//       invoiceTypeEnum,
//       invoiceTypeEnumV,
//       transportModeEnum,
//       transportModeEnumV,
//       engineerSettlementTypeEnum,
//       engineerSettlementTypeEnumV,
//       enclosureSettlementTypeEnum,
//       enclosureSettlementTypeEnumV,
//       projectTypeEnumN,
//       businessTypeEnum,
//       projectContent,
//       projectContentME,
//       projectTypeEnumV,
//       businessTypeEnumV,
//       projectContentV,
//       projectContentMEV,
//       maxNubmer: 99999999999,
//       // isModify: false,
//       submitLoading: false,
//       form: {},
//       detail: {},
//       rules: {
//         paymentDescription: [
//           { max: 2000, message: '不能超过 2000 个字符', trigger: 'blur' }
//         ],
//         businessType: [{ required: true, message: '请选择业务类型', trigger: 'change' }],
//         projectType: [{ required: true, message: '请选择项目类型', trigger: 'change' }],
//         projectContent: [{ required: true, message: '请输入项目内容', trigger: 'change' }],
//         contractSignBodyId: [{ required: true, message: '请选择合同签订主体（签订主体可在配置管理-基础配置-分支机构中创建）', trigger: 'change' }]
//       },
//       tableRules: {
//         productCategory: [{ required: true, max: 100, message: '不能超过 100 个字符', trigger: 'blur' }],
//         engineeringMete: [{ required: true, message: '请填写工程量', trigger: 'blur', type: 'number' }],
//         unit: [{ required: true, max: 10, message: '不能超过 10 个字符', trigger: 'blur' }],
//         unitPrice: [{ required: true, message: '请填写单价', trigger: 'blur', type: 'number' }],
//         remark: [{ max: 500, message: '不能超过 500 个字符', trigger: 'blur' }]
//       },
//       projectContentOption: [],
//       enclosureVisible: false,
//       showItem: [],
//       showCategory: [],
//       originConstruct: []
//     }
//   },
//   computed: {
//     ...mapGetters(['globalProjectId']),
//     currentProjectId() {
//       return this.projectId || this.globalProjectId
//     },
//     isModify() {
//       return this.showEdit || false
//     }
//   },
//   watch: {
//     currentProjectId: {
//       handler(val) {
//         // this.isModify = false
//         this.fetchDetail()
//       },
//       deep: true
//     },
//     isModify: {
//       handler(val) {
//         this.openEdit()
//       }
//     }
//   },
//   created() {
//     this.fetchDetail()
//   },
//   methods: {
//     getshowItem(val) {
//       this.showItem = []
//       this.showCategory = []
//       const totalArr = [1, 2, 3, 4]
//       if (val.length > 0) {
//         val.map(v => {
//           if (this.form.businessType === 1) {
//             const val = this.projectContent1.find(k => k.id === v)
//             if (val.alias === 'STRUCTURE') {
//               if (this.showItem.indexOf(5) < 0) {
//                 this.showItem.push(5)
//               }
//               this.showCategory.push(val)
//             } else {
//               if (totalArr.indexOf(Number(val.no)) > -1 && this.showItem.indexOf(Number(val.no)) < 0) {
//                 this.showItem.push(Number(val.no))
//               }
//             }
//           } else {
//             const val = this.projectContent2.find(k => k.id === v)
//             if (val.alias) {
//               if (val.alias === 'STRUCTURE') {
//                 if (this.showItem.indexOf(5) < 0) {
//                   this.showItem.push(5)
//                   this.showCategory = this.originConstruct
//                 }
//               } else if (val.alias === 'ENCLOSURE') {
//                 this.showItem = [...this.showItem, ...totalArr]
//               }
//             }
//           }
//         })
//       }
//     },
//     enclosureSave() {
//       const info = this.$refs.enclosureForm.tableData
//       this.form = {
//         ...this.form,
//         enclosureInfo: info,
//         structureSaveRequestVOS: info[enclosureTypeEnum.STRUCTURE.V],
//         profiledPlateSaveRequestVOS: info[enclosureTypeEnum.PROFILEDPLATE.V],
//         pressureBearingPlateSaveVOS: info[enclosureTypeEnum.PRESSUREBEARINGPLATE.V],
//         trussFloorPlateSaveRequestVOS: info[enclosureTypeEnum.TRUSSFLOORPLATE.V],
//         sandwichBoardSaveRequestVOS: info[enclosureTypeEnum.SANDWICH_BOARD.V]
//       }
//       this.enclosureVisible = false
//     },
//     businessChange() {
//       this.projectContentOption = []
//       this.form.projectContent = []
//       if (this.form.businessType) {
//         this.projectContentOption = this.form.businessType === 2 ? this.projectContent2 : this.projectContent1
//       }
//     },
//     downloadSettlementAttachments, // TODO:
//     openEdit() {
//       this.resetForm(this.detail)
//       this.form.projectContent = []
//       this.projectContentOption = this.form.businessType === 2 ? this.projectContent2 : this.projectContent1
//       if (this.detail.projectContentList && this.detail.projectContentList.length > 0) {
//         this.detail.projectContentList.forEach(v => {
//           this.form.projectContent.push(v.id)
//         })
//       }
//       this.getshowItem(this.form.projectContent)
//       // this.isModify = true
//     },
//     cancelEdit() {
//       this.$emit('changeStatus', false)
//       // this.isModify = false
//       // reset info
//     },
//     async submit() {
//       this.submitLoading = true
//       this.cancelEdit()
//       // try {
//       //   const valid = await this.validate()
//       //   if (valid) {
//       //     let _isModify = false
//       //     if (this.form.businessList.length !== this.detail.businessList.length) {
//       //       _isModify = true
//       //     } else {
//       //       for (let i = 0; i < this.form.businessList.length; i++) {
//       //         const _formObj = Object.assign({}, this.form.businessList[i])
//       //         const _detailObj = Object.assign({}, this.detail.businessList[i])
//       //         delete _formObj.verify
//       //         if (!isObjectValueEqual(_formObj, _detailObj)) {
//       //           _isModify = true
//       //           break
//       //         }
//       //       }
//       //     }
//       //     this.form.isModify = _isModify
//       //     const data = JSON.parse(JSON.stringify(this.form))
//       //     data.settlementAttachments = data.settlementAttachments && data.settlementAttachments.map(v => v.id)
//       //     await edit(data)
//       //     this.$notify({ title: '修改成功', type: 'success', duration: 2500 })
//       //     this.cancelEdit()
//       //     this.fetchDetail()
//       //   }
//       // } catch (error) {
//       //   console.log('error', error)
//       // } finally {
//       //   this.submitLoading = false
//       // }
//     },
//     fetchDetail: debounce(async function() {
//       if (!checkPermission(permission.get) || !this.currentProjectId) {
//         return
//       }
//       const loading = this.$loading({
//         target: '#pageContainer',
//         lock: true,
//         text: '请稍后，正在加载合同基础信息',
//         fullscreen: false
//       })
//       let _detail = {}
//       try {
//         const res = await getDetail(this.currentProjectId)
//         if (!res.enclosureMeasureMode) {
//           res.enclosureMeasureMode = enclosureSettlementTypeEnum.LENGTH.V
//         }
//         _detail = JSON.parse(JSON.stringify(res))
//         _detail.settlementAttachments = _detail.settlementAttachments || []
//         _detail.businessList = _detail.businessList || []
//         _detail.businessList = _detail.businessList.map(b => {
//           b.totalAmount = (b.engineeringMete || 0) * (b.unitPrice || 0).toFixed(this.$DP.YUAN)
//           return b
//         })
//         const data = await getTechInfo(this.currentProjectId)
//         _detail.enclosureInfo = {
//           [enclosureTypeEnum.STRUCTURE.V]: data.structureList ? data.structureList : [],
//           [enclosureTypeEnum.PROFILEDPLATE.V]: data.profiledPlateList ? data.profiledPlateList : [],
//           [enclosureTypeEnum.TRUSSFLOORPLATE.V]: data.trussFloorPlateList ? data.trussFloorPlateList : [],
//           [enclosureTypeEnum.PRESSUREBEARINGPLATE.V]: data.pressureBearingPlateList ? data.pressureBearingPlateList : [],
//           [enclosureTypeEnum.SANDWICH_BOARD.V]: data.sandwichBoardList ? data.sandwichBoardList : []
//         }
//         const options = []
//         this.originConstruct = []
//         const data1 = await getContentInfo({ businessType: 1 })
//         const data2 = await getContentInfo({ businessType: 2 })
//         if (data1 && data1.projectContentVOList.length > 0) {
//           data1.projectContentVOList.forEach(v => {
//             if (v.contentList.length > 0) {
//               v.contentList.forEach(k => {
//                 k.alias = v.type
//                 options.push(k)
//                 if (k.alias === 'STRUCTURE') {
//                   if (this.originConstruct.indexOf(k) < 0) {
//                     this.originConstruct.push(k)
//                   }
//                 }
//               })
//             }
//           })
//         }
//         this.projectContent1 = options || []
//         this.projectContent2 = data2.projectContentVOList || []
//       } catch (error) {
//         console.log('error', error)
//       } finally {
//         this.detail = _detail
//         this.resetForm(this.detail)
//         this.form.projectContent = []
//         this.projectContentOption = this.form.businessType === 2 ? this.projectContent2 : this.projectContent1
//         if (this.detail.projectContentList && this.detail.projectContentList.length > 0) {
//           this.detail.projectContentList.forEach(v => {
//             this.form.projectContent.push(v.id)
//           })
//         }
//         this.getshowItem(this.form.projectContent)
//         loading.close()
//       }
//     }, 100, false),
//     calcTotalAmount(row) {
//       row.totalAmount = (row.engineeringMete || 0) * (row.unitPrice || 0).toFixed(this.$DP.YUAN)
//     },
//     getSummaries(param) {
//       const { columns, data } = param
//       const sums = []
//       columns.forEach((column, index) => {
//         if (index === 0) {
//           sums[index] = '合计'
//           return
//         }
//         if ([1, 2, 3, 4, 6, 7].includes(index)) { // 种类 备注 操作
//           sums[index] = 'N/A'
//           return
//         }
//         const values = data.map(item => Number(item[column.property]))
//         if (!values.every(value => isNaN(+value))) {
//           sums[index] = values.reduce((prev, curr) => {
//             const value = Number(curr)
//             if (!isNaN(value)) {
//               return prev + curr
//             } else {
//               return prev
//             }
//           }, 0)
//         } else {
//           sums[index] = 'N/A'
//         }
//       })

//       return sums
//     },
//     handleExcelSuccess(val) {
//       if (val && val.length > 3) {
//         const data = val.slice(2, val.length - 1) || []
//         data.map(v => {
//           if (v) {
//             const obj = {}
//             for (const key in excelField) {
//               obj[key] = v[excelField[key]]
//             }
//             this.calcTotalAmount(obj)
//             this.form.businessList.push(obj)
//           }
//         })
//       }
//     },
//     handleTaxChage(val) {
//       if (val) {
//         this.form.invoiceType = invoiceTypeEnum.SPECIAL_INVOICE.V
//       } else {
//         this.form.invoiceType = undefined
//       }
//     },
//     async validate() {
//       try {
//         const valid = await this.$refs.form.validate()
//         const tableValid = this.validateTable()
//         return valid && tableValid
//       } catch (error) {
//         console.log('error', error)
//         return false
//       }
//     },
//     // 添加行
//     addRow: function() {
//       this.form.businessList.push({ verify: {}})
//     },
//     // 删除行
//     deleteRow: function(index) {
//       this.form.businessList.splice(index, 1)
//     },
//     excelImport: function() {
//       console.log('excel导入')
//     },
//     /**
//      * 重置表单
//      * @param {Array} data 数据
//      */
//     resetForm(data) {
//       // 清除表单信息
//       if (this.$refs['form']) {
//         this.$refs['form'].resetFields()
//       }
//       let form
//       if (data && Object.keys(data).length > 0) {
//         form = JSON.parse(JSON.stringify(data))
//       } else {
//         form = JSON.parse(JSON.stringify(defaultForm))
//       }
//       // const form = data || JSON.parse(JSON.stringify(defaultForm))
//       const crudFrom = this.form = JSON.parse(JSON.stringify(defaultForm))
//       // const crudFrom = this.form
//       // for (const key in crudFrom) {
//       //   crudFrom[key] = undefined
//       // }
//       for (const key in form) {
//         if (crudFrom.hasOwnProperty(key)) {
//           crudFrom[key] = form[key]
//         } else {
//           this.$set(crudFrom, key, form[key])
//         }
//       }
//     },
//     validateTable() {
//       let flag = true
//       const list = this.form.businessList
//       let message = '请填写数据'
//       if (list && list.length > 0) {
//         // TODO: 考虑封装
//         list.forEach(row => {
//           for (const rule in this.tableRules) {
//             row.verify = row.verify ? row.verify : {}
//             row.verify[rule] = tableValidate(this.tableRules[rule], row[rule])
//             if (!row.verify[rule]) {
//               flag = false
//             }
//           }
//         })
//         if (!flag) {
//           this.form.businessList = Object.assign([], list)
//           message = '请修正表格中标红的信息, 填写限制请参照下方“商务信息列表填写须知”'
//         }
//       }
//       if (!flag) {
//         this.$message({ message, type: 'error' })
//       }
//       return flag
//     },
//     handelCellClassName({ row, column, rowIndex, columnIndex }) {
//       let flag = true
//       if (row.verify && Object.keys(row.verify) && Object.keys(row.verify).length > 0) {
//         if (row.verify[column.property] === false) {
//           flag = tableValidate(this.tableRules[column.property], row[column.property])
//         }
//         if (flag) {
//           row.verify[column.property] = true
//         }
//       }
//       return flag ? '' : 'mask-td'
//     }
//   }
// }
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
>>> .input-underline {
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
>>> .el-input-number .el-input__inner {
  text-align: left;
}
.form-row {
  width: 100%;
}
span {
  // color:#4482ff #1682e6
  color: #82848a;
}
</style>
