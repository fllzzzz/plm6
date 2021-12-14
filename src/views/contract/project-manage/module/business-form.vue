<template>
  <el-form
    ref="businessRef"
    :model="form"
    :rules="rules"
    inline
    size="small"
    label-position="right"
    label-width="130px"
  >
    <div>
      <div id="baseInfo">
        <div class="form-row">
          <el-form-item label="合同签订主体" prop="contractSignBodyId">
            <branch-company-select
              v-model="form.contractSignBodyId"
              default
              class="input-underline"
              placeholder="合同签订主体"
              style="width:550px"
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
          <el-form-item label="项目类型" prop="projectType">
            <common-select
              v-model="form.projectType"
              :options="projectTypeEnumN.ENUM"
              type="enum"
              size="small"
              clearable
              placeholder="项目类型"
              style="width:200px"
              class="input-underline"
            />
          </el-form-item>
          <el-form-item label="项目内容" prop="projectContent">
            <el-select v-model="form.projectContent" multiple placeholder="项目内容,可多选" class="input-underline" style="width:320px" @change="getshowItem">
              <el-option
                v-for="item in projectContentOption"
                :key="item.id"
                :label="item.name"
                :value="item.id"
              />
            </el-select>
          </el-form-item>
        </div>
        <div class="form-row">
          <el-form-item label="签约人" prop="singerId">
            <user-dept-cascader
              v-model="form.singerId"
              filterable
              :collapse-tags="false"
              clearable
              show-all-levels
              class="input-underline"
              style="width:200px"
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
              style="width:200px"
            />
          </el-form-item>
          <el-form-item label="签约地址" prop="signingAddress">
            <el-input
              v-model="form.signingAddress"
              class="input-underline"
              placeholder="签约地址"
              style="width:320px"
            />
          </el-form-item>
        </div>
        <div class="form-row">
        <!-- <div v-if="!(props.projectType & projectTypeEnumN.ENUM.ENCLOSURE.V)" class="form-row"> -->
          <el-form-item label="工程结算方式" prop="settlementType">
            <common-radio
              v-model="form.settlementType"
              :options="engineerSettlementTypeEnumN.ENUM"
              type="enum"
            />
          </el-form-item>
          <el-form-item label="围护结算方式" prop="enclosurePriceType">
            <common-radio
              v-model="form.enclosurePriceType"
              :options="enclosureSettlementTypeEnum.ENUM"
              type="enum"
            />
          </el-form-item>
        </div>
        <div class="form-row">
          <el-form-item label="运输方式" prop="transportMode">
            <common-radio
              v-model="form.transportMode"
              :options="transportModeEnum.ENUM"
              type="enum"
            />
          </el-form-item>
          <el-form-item label="支付方式" prop="paymentMode">
            <common-radio
              v-model="form.paymentMode"
              :options="paymentModeEnum.ENUM"
              type="enum"
            />
          </el-form-item>
        </div>
        <div class="form-row">
          <el-form-item label="合同含税" prop="isTaxInclusive">
            <common-radio
              v-model="form.isTaxInclusive"
              :options="isTaxEnum.ENUM"
              type="enum"
            />
          </el-form-item>
          <el-form-item label="发票类型" prop="invoiceType">
            <common-select
              type="enum"
              size="small"
              v-model="form.invoiceType"
              :options="invoiceTypeEnum.ENUM"
              :disabled="!form.isTaxInclusive"
              class="input-underline"
              placeholder="选择发票类型"
              style="width: 200px;"
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
          <el-form-item label="支付方式描述" prop="paymentDescription">
            <el-input
              v-model="form.paymentDescription"
              type="textarea"
              :autosize="{ minRows: 4, maxRows: 4}"
              class="input-underline"
              style="width:550px"
              placeholder="付款方式描述"
            />
          </el-form-item>
        </div>
      </div>
      <el-divider><span class="title">技术交底</span></el-divider>
      <div style="text-align:right;margin-right:20px;">
        <common-button
          style="margin-left:20px;"
          type="success"
          size="small"
          @click="handleAddEnclosure"
        >添加</common-button>
      </div>
      <enclosure-show :table-data="form.enclosureInfo" :show-item="showItem" />
      <!--围护产品数据弹窗  -->
      <common-drawer
        :visible.sync="enclosureVisible"
        :with-header="true"
        :show-close="false"
        :wrapper-closable="false"
        direction="rtl"
        size="80%"
        :before-close="() => {enclosureVisible = false}"
      >
        <template #title>
          <span class="line-title">添加技术交底</span>
          <span>
            <common-button v-if="showItem && showItem.length>0" size="small" type="primary" @click="enclosureSave">保存</common-button>
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
import { ref, defineProps, watch, computed } from 'vue'
import userDeptCascader from '@comp-base/user-dept-cascader.vue'
import branchCompanySelect from '@comp-base/branch-company-select.vue'
import useWatchFormValidate from '@compos/form/use-watch-form-validate'
import { projectTypeEnumN, businessTypeEnum, paymentModeEnum, invoiceTypeEnum, isTaxEnum, engineerSettlementTypeEnumN, enclosureSettlementTypeEnum, transportModeEnum, TechnologyTypeEnum } from '@enum-ms/contract'
import { getContentInfo } from '@/api/contract/project'
import { ElMessage } from 'element-plus'
import enclosureForm from './enclosure-form'
import enclosureShow from './enclosure-show'

const businessRef = ref()
let projectContent1 = []
let projectContent2 = []
let originConstruct = []
const projectContentOption = ref([])
const showItem = ref([])
const showCategory = ref([])
const enclosureVisible = ref(false)
const enclosureFormRef = ref()
const props= defineProps({
  formData: {
    type: Object,
    default: () => {}
  },
  projectType: {
    type: Number,
    default: undefined
  }
})
const defaultForm = {
  contractSignBodyId: undefined, // 合同签订主体
  businessType: undefined, // 业务类型
  projectType: undefined, // 项目类型
  projectContent: [], // 项目内容
  singerId: undefined, // 签约人
  signingDate: undefined, // 签约日期
  signingAddress: undefined, // 签约地址
  settlementType: engineerSettlementTypeEnumN.THEORY.V, // 结算方式
  enclosurePriceType: enclosureSettlementTypeEnum.LENGTH.V, // 围护结算方式
  transportMode: transportModeEnum.HOME_DELIVERY.V, // 运输方式
  paymentMode: paymentModeEnum.PUBLIC_TRANSFER.V, // 付款方式
  isTaxInclusive: true, // 是否含税
  invoiceType: invoiceTypeEnum.SPECIAL.V, // 发票类型
  paymentDescription: undefined, // 付款方式描述
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

const form=ref(JSON.parse(JSON.stringify(defaultForm)))
const rules = {
  paymentDescription: [
    { max: 2000, message: '不能超过 2000 个字符', trigger: 'blur' }
  ],
  businessType: [{ required: true, message: '请选择业务类型', trigger: 'change' }],
  projectType: [{ required: true, message: '请选择项目类型', trigger: 'change' }],
  projectContent: [{ required: true, message: '请输入项目内容', trigger: 'change' }],
  contractSignBodyId: [{ required: true, message: '请选择合同签订主体（签订主体可在配置管理-基础配置-分支机构中创建）', trigger: 'change' }]
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
  if (businessRef.value) {
    businessRef.value.resetFields()
  }
  let formKey
  if (data && Object.keys(data).length > 0) {
    formKey = data
  } else {
    formKey = JSON.parse(JSON.stringify(defaultForm))
    Object.assign(form.value, JSON.parse(JSON.stringify(techForm)))
  }
  const crudFrom = form.value
  for (const key in crudFrom) {
    crudFrom[key] = undefined
  }
  for (const key in formKey) {
    crudFrom[key] = formKey[key]
  }
  useWatchFormValidate(businessRef, form.value)
}

contentInfo()

async function contentInfo() {
  try {
    const options = []
    const data1 = await getContentInfo({ businessType: businessTypeEnum.ENUM.MACHINING.V })
    const data2 = await getContentInfo({ businessType: businessTypeEnum.ENUM.INSTALLATION.V })
    if (data1 && data1.projectContentVOList.length > 0) {
      data1.projectContentVOList.forEach(v => {
        if (v.contentList.length > 0) {
          v.contentList.forEach(k => {
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

function getshowItem(val) {
  showItem.value = []
  showCategory.value = []
  const totalArr = [TechnologyTypeEnum.ENUM.SANDWICH_BOARD.V, TechnologyTypeEnum.ENUM.PROFILEDPLATE.V, TechnologyTypeEnum.ENUM.TRUSSFLOORPLATE.V, TechnologyTypeEnum.ENUM.PRESSUREBEARINGPLATE.V]
  if (val.length > 0) {
    val.map(v => {
      if (form.value.businessType === businessTypeEnum.ENUM.MACHINING.V) {
        const val = projectContent1.find(k => k.id === v)
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
        const val = projectContent2.find(k => k.id === v)
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
    sandwichBoardSaveRequestVOS: info[TechnologyTypeEnum.ENUM.SANDWICH_BOARD.V]
  }
  enclosureVisible.value = false
}

async function validateForm() {
  try {
    const valid = await businessRef.value.validate()
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
// import enclosureForm from './enclosure-form'
// import enclosureShow from './enclosure-show'
// import branchCompanySelect from '@/views/components/base/branch-company-select'
// import userDeptCascader from '@/views/components/base/user-dept-cascader'
// import { paymentModeEnum, projectTypeEnum, invoiceTypeEnum, engineerSettlementTypeEnum, transportModeEnum, enclosureSettlementTypeEnum, projectTypeEnumN, businessTypeEnum, projectContent, projectContentME, TechnologyTypeEnum as typeEnum } from '@/utils/enum/index'
// import { tableValidate } from '@/utils/other'
// import { getContentInfo } from '@/api/contract/project'

// const defaultForm = {
//   contractSignBodyId: undefined, // 合同签订主体
//   businessType: undefined, // 业务类型
//   projectType: undefined, // 项目类型
//   projectContent: [], // 项目内容
//   singerId: undefined, // 签约人
//   signingDate: undefined, // 签约日期
//   signingAddress: undefined, // 签约地址
//   paymentMode: paymentModeEnum.PUBLIC_TRANSFER.V, // 付款方式
//   isTaxInclusive: true, // 是否含税
//   invoiceType: invoiceTypeEnum.SPECIAL_INVOICE.V, // 发票类型
//   paymentDescription: undefined, // 付款方式描述
//   enclosureInfo: {},
//   structureSaveRequestVOS: [],
//   profiledPlateSaveRequestVOS: [],
//   pressureBearingPlateSaveVOS: [],
//   trussFloorPlateSaveRequestVOS: [],
//   sandwichBoardSaveRequestVOS: []
// }
// const techForm = {
//   enclosureInfo: {},
//   structureSaveRequestVOS: [],
//   profiledPlateSaveRequestVOS: [],
//   pressureBearingPlateSaveVOS: [],
//   trussFloorPlateSaveRequestVOS: [],
//   sandwichBoardSaveRequestVOS: []
// }
// const excelField = {
//   productCategory: '__EMPTY', // 产品种类
//   engineeringMete: '__EMPTY_1', // 工程量
//   unit: '__EMPTY_2', // 单位
//   unitPrice: '__EMPTY_3', // 单价
//   remark: '__EMPTY_5' // 备注
// }

// export default {
//   components: { branchCompanySelect, userDeptCascader, enclosureForm, enclosureShow },
//   inject: ['permission'],
//   props: {
//     formData: {
//       type: Object,
//       default: () => {}
//     },
//     projectType: {
//       type: Number,
//       default: undefined
//     }
//   },
//   data() {
//     return {
//       paymentModeEnum,
//       invoiceTypeEnum,
//       transportModeEnum,
//       projectTypeEnum,
//       engineerSettlementTypeEnum,
//       enclosureSettlementTypeEnum,
//       projectTypeEnumN,
//       businessTypeEnum,
//       projectContent,
//       projectContentME,
//       projectContent1: [],
//       projectContent2: [],
//       projectContentOption: [],
//       maxNubmer: 99999999999,
//       form: {},
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
//       enclosureVisible: false,
//       showItem: [],
//       showCategory: [],
//       originConstruct: []
//     }
//   },
//   watch: {
//     formData: {
//       handler(val) {
//         this.resetForm(val)
//         this.contentInfo()
//       },
//       deep: true
//     }
//   },
//   created() {
//     this.resetForm()
//     this.resetForm(this.formData)
//     this.contentInfo()
//   },
//   methods: {
//     async contentInfo() {
//       try {
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
//         console.log(error)
//       }
//     },
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
//     handleAddEnclosure() {
//       if (!this.form.projectContent || this.form.projectContent.length === 0) {
//         this.$message.error('请先选择项目内容')
//         return
//       }
//       this.enclosureVisible = true
//     },
//     businessChange() {
//       this.projectContentOption = []
//       this.form.projectContent = []
//       this.showItem = []
//       this.showCategory = []
//       Object.assign(this.form, JSON.parse(JSON.stringify(techForm)))
//       if (this.form.businessType) {
//         this.projectContentOption = this.form.businessType === 1 ? this.projectContent1 : this.projectContent2
//       }
//     },
//     // 围护保存
//     enclosureSave() {
//       const info = this.$refs.enclosureForm.tableData
//       this.form = {
//         ...this.form,
//         enclosureInfo: info,
//         structureSaveRequestVOS: info[typeEnum.STRUCTURE.V],
//         profiledPlateSaveRequestVOS: info[typeEnum.PROFILEDPLATE.V],
//         pressureBearingPlateSaveVOS: info[typeEnum.PRESSUREBEARINGPLATE.V],
//         trussFloorPlateSaveRequestVOS: info[typeEnum.TRUSSFLOORPLATE.V],
//         sandwichBoardSaveRequestVOS: info[typeEnum.SANDWICH_BOARD.V]
//       }
//       this.enclosureVisible = false
//     },
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
//           sums[index]
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
//         if (valid && tableValid) {
//           Object.assign(this.formData, JSON.parse(JSON.stringify(this.form)))
//         }
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
//         form = data
//       } else {
//         form = JSON.parse(JSON.stringify(defaultForm))
//         Object.assign(this.form, JSON.parse(JSON.stringify(techForm)))
//       }
//       // const form = data || JSON.parse(JSON.stringify(defaultForm))
//       const crudFrom = this.form
//       for (const key in crudFrom) {
//         crudFrom[key] = undefined
//       }
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
.add-row-box {
  display: flex;
  flex-direction: row;
  justify-content: center;
  align-items: center;
  margin-top: 20px;
}
>>>.input-underline {
  // width: calc((95vw - 40px)/3);
  width: 200px;
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
</style>
