<template>
  <div class="steel-requisitions-application-container">
    <common-wrapper
      :basic-class="STEEL_ENUM"
      :current-basic-class="steelBasicClassKV?.[currentBasicClass]?.V"
      :total-value="totalWeight"
      :show-total-amount="true"
      :validate="validate"
      unit="kg"
      total-name="总量合计"
    >
      <div class="filter-container">
        <div class="filter-left-box">
          <el-radio-group v-model="currentBasicClass" size="small" class="filter-item">
            <el-radio-button v-for="item in steelBasicClassKV" :key="item.K" :label="item.K" :disabled="item.V === matClsEnum.STEEL_COIL.V">
              {{ item.L }}{{ form[item.K] && form[item.K].length ? `(${form[item.K].length})` : '' }}
            </el-radio-button>
          </el-radio-group>
        </div>
        <div class="filter-right-box">
          <common-button class="filter-item" type="success" @click="materialSelectVisible = true"> 添加物料 </common-button>
          <excel-resolve-button
            icon="el-icon-upload2"
            btn-name="清单导入"
            btn-size="small"
            class="filter-item"
            btn-type="warning"
            open-loading
            :template="importTemp"
            @success="handleExcelSuccess"
          />
          <export-button
            v-if="currentBasicClass"
            class="filter-item"
            size="small"
            type="info"
            :params="{ basicClass: steelBasicClassKV[currentBasicClass].V, receiptType: receiptTypeEnum.REQUISITIONS.V }"
            :fn="downloadExcelTemplate"
          >
            {{ steelBasicClassKV[currentBasicClass].L }}清单模板下载
          </export-button>
        </div>
      </div>
      <el-form ref="formRef" :model="form">
        <component ref="steelRef" :max-height="tableMaxHeight" :style="maxHeightStyle" :is="comp" @search-inventory="searchInventory" />
      </el-form>
    </common-wrapper>
    <common-drawer
      ref="drawerRef"
      v-model="materialSelectVisible"
      title="物料选择"
      :show-close="true"
      :size="900"
      custom-class="material-table-spec-select"
    >
      <template #content>
        <material-table-spec-select
          ref="matSpecRef"
          v-model="list"
          :visible="materialSelectVisible"
          :row-init-fn="rowInit"
          :max-height="specSelectMaxHeight"
          :basic-class="steelBasicClassKV?.[currentBasicClass]?.V"
          :table-width="350"
          auto-selected
          expand-query
        />
      </template>
    </common-drawer>
    <inventory-drawer v-model:visible="inventoryVisible" :params="searchInfo" @use-inventory="useInventory" />
  </div>
</template>

<script setup>
import crudApi from '@/api/supply-chain/requisitions-manage/requisitions'
import { downloadExcelTemplate } from '@/api/wms/common'

import { defineProps, defineEmits, ref, computed, watch, provide, nextTick, reactive } from 'vue'
import { STEEL_ENUM } from '@/settings/config'
import { convertUnits } from '@/utils/convert/unit'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { steelInboundFormFormat } from '@/utils/wms/measurement-calc'
import { preparationTypeEnum, requisitionModeEnum, receiptTypeEnum } from '@enum-ms/wms'
import steelPlateTemp from '@/utils/excel/import-template/supply-chain/requisition-temp/steel-plate'
import sectionSteelTemp from '@/utils/excel/import-template/supply-chain/requisition-temp/section-steel'
import steelCoilTemp from '@/utils/excel/import-template/supply-chain/requisition-temp/steel-coil'

import useMatBaseUnit from '@/composables/store/use-mat-base-unit'
import useForm from '@/composables/form/use-form'
import useMaxHeight from '@compos/use-max-height'
import commonWrapper from './../components/common-wrapper.vue'
import materialTableSpecSelect from '@/components-system/classification/material-table-spec-select.vue'
import steelPlateTable from './module/steel-plate-table.vue'
import sectionSteelTable from './module/section-steel-table.vue'
import steelCoilTable from './module/steel-coil-table.vue'
import inventoryDrawer from '../components/inventory-drawer'
import excelResolveButton from '@/components-system/common/excel-resolve-button/index.vue'
import ExportButton from '@comp-common/export-button/index.vue'
import { ElMessage, ElRadioGroup } from 'element-plus'
import { isBlank, isNotBlank, toFixed } from '@/utils/data-type'

const emit = defineEmits(['success'])

const props = defineProps({
  detail: {
    type: Object
  },
  isEdit: {
    type: Boolean,
    default: false
  }
})

const { baseUnit } = useMatBaseUnit() // 当前分类基础单位

// 基础分类
const steelBasicClassKV = {
  steelPlateList: { K: 'steelPlateList', L: '钢板', V: matClsEnum.STEEL_PLATE.V, TABLE: steelPlateTable },
  sectionSteelList: { K: 'sectionSteelList', L: '型材', V: matClsEnum.SECTION_STEEL.V, TABLE: sectionSteelTable },
  steelCoilList: { K: 'steelCoilList', L: '钢卷', V: matClsEnum.STEEL_COIL.V, TABLE: steelCoilTable }
}

const defaultForm = {
  list: [], // 钢材列表，提交时合并
  steelPlateList: [], // 钢板列表
  sectionSteelList: [], // 型材列表
  steelCoilList: [] // 钢卷列表
}

const steelRef = ref() // 表格ref
const matSpecRef = ref() // 规格列表ref
const formRef = ref() // form表单ref
const drawerRef = ref()
const materialSelectVisible = ref(false) // 显示物料选择
const inventoryVisible = ref(false)
const searchInfo = ref({})
const searchIdx = ref()
const currentBasicClass = ref() // 当前基础分类
const list = ref([]) // 当前操作的表格list

// 钢材三个组件的ref列表
const steelRefList = reactive({
  steelPlateList: null,
  sectionSteelList: null,
  steelCoilList: null
})

provide('matSpecRef', matSpecRef) // 供兄弟组件调用 删除

// 使用草稿时，为数据设置监听
const setFormCallback = (form) => {
  const trigger = {
    steelPlateList: null,
    sectionSteelList: null,
    steelCoilList: null
  }
  const initSelectedTrigger = {
    steelPlateList: null,
    sectionSteelList: null,
    steelCoilList: null
  }
  const list = ['steelPlateList', 'sectionSteelList', 'steelCoilList']
  let hasDefaultSelect = false // 是否有默认选中，优先选中有数据的类型
  list.forEach((key) => {
    if (isNotBlank(form[key])) {
      if (!hasDefaultSelect) {
        hasDefaultSelect = true
        currentBasicClass.value = key
      }

      form[key] = form[key].map((v) => reactive(v))
      trigger[key] = watch(
        steelRefList,
        (ref) => {
          if (ref[key]) {
            // 初始化数据监听，执行一次后取消当前监听
            form[key].forEach((v) => ref[key].rowWatch(v))
            // 初始化选中数据，执行一次后取消当前监听
            initSelectedTrigger[key] = watch(
              matSpecRef,
              () => {
                if (matSpecRef.value) {
                  matSpecRef.value.initSelected(
                    form[key].map((v) => {
                      return { sn: v.sn, classifyId: v.classifyId }
                    })
                  )
                  nextTick(() => {
                    initSelectedTrigger[key]()
                  })
                }
              },
              { immediate: true }
            )
            nextTick(() => {
              trigger[key]()
            })
          }
        },
        { immediate: true, deep: true }
      )
    }
  })
}

const { cu, form, FORM } = useForm(
  {
    title: '钢材申购',
    // formStore: !props.isEdit,
    // formStoreKey: 'REQUISITIONS_STEEL',
    defaultForm: defaultForm,
    useDraftCallback: setFormCallback,
    clearDraftCallback: init,
    api: props.isEdit ? crudApi.edit : crudApi.add
  },
  formRef,
  props.detail
)

// 当前物料“批量导入模板”
const importTemp = computed(() => {
  switch (currentBasicClass.value) {
    case steelBasicClassKV.steelPlateList.K:
      return steelPlateTemp
    case steelBasicClassKV.sectionSteelList.K:
      return sectionSteelTemp
    case steelBasicClassKV.steelCoilList.K:
      return steelCoilTemp
    default:
      return steelPlateTemp
  }
})

watch(
  () => props.detail,
  (val = {}) => {
    form.type = val.type
    form.materialType = val.materialType
    form.projectId = val.projectId
  },
  { deep: true, immediate: true }
)

// 物料选择组件高度
const { maxHeight: specSelectMaxHeight } = useMaxHeight(
  {
    mainBox: '.material-table-spec-select',
    extraBox: '.el-drawer__header',
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true,
    minHeight: 300
  },
  () => drawerRef.value.loaded
)

const { maxHeight: tableMaxHeight, maxHeightStyle } = useMaxHeight({
  mainBox: '.requisitions-application-record-form',
  extraBox: [
    '.el-drawer__header',
    '.filter-container',
    '.requisitions-application-header',
    '.requisitions-application-select',
    '.requisitions-application-footer'
  ],
  wrapperBox: ['.el-drawer__body'],
  clientHRepMainH: true,
  navbar: false,
  minHeight: 300,
  extraHeight: 0
})

// 当前使用的组件
const comp = computed(() => {
  return steelBasicClassKV?.[currentBasicClass.value]?.TABLE || steelBasicClassKV.steelPlateList.TABLE
})

// 总重
const totalWeight = computed(() => {
  let weight = 0
  if (isNotBlank(form.steelPlateList)) {
    form.steelPlateList.forEach((v) => {
      weight += v.weighingTotalWeight ? v.weighingTotalWeight : 0
    })
  }
  if (isNotBlank(form.sectionSteelList)) {
    form.sectionSteelList.forEach((v) => {
      weight += v.weighingTotalWeight ? v.weighingTotalWeight : 0
    })
  }
  if (isNotBlank(form.steelCoilList)) {
    form.steelCoilList.forEach((v) => {
      weight += v.weighingTotalWeight ? v.weighingTotalWeight : 0
    })
  }
  return toFixed(weight, 2)
})

// 监听切换钢材类型，为list赋值
watch(
  currentBasicClass,
  (k) => {
    list.value = form[k]
    if (k) {
      nextTick(() => {
        // nextTick 后 steelRef.value 才会发生变化
        if (!steelRefList[k]) steelRefList[k] = steelRef.value
      })
    }
  },
  { immediate: true }
)

// 监听list变更,为对应的钢材清单赋值，监听地址即可
watch(
  list,
  (val) => {
    form[currentBasicClass.value] = val
  },
  { deep: true }
)

function searchInventory(row, index) {
  searchInfo.value = row
  searchIdx.value = index
  inventoryVisible.value = true
}

function useInventory(quantity, data) {
  const _curBasicClass = steelBasicClassKV?.[currentBasicClass.value]?.V
  if (_curBasicClass & matClsEnum.STEEL_PLATE.V) {
    // 钢板冻结钢卷
    if (data.basicClass & matClsEnum.STEEL_COIL.V) {
      form[currentBasicClass.value][searchIdx.value].length = convertUnits(
        quantity,
        baseUnit.value[data.basicClass]?.measure?.unit,
        baseUnit.value[_curBasicClass].length.unit,
        baseUnit.value[_curBasicClass].length.precision
      )
      form[currentBasicClass.value][searchIdx.value].quantity = 1
      form[currentBasicClass.value][searchIdx.value].canUseQuantity = 1
    } else {
      form[currentBasicClass.value][searchIdx.value].length = data.length
      form[currentBasicClass.value][searchIdx.value].quantity = quantity
      form[currentBasicClass.value][searchIdx.value].canUseQuantity = data.quantity
    }
    form[currentBasicClass.value][searchIdx.value].width = data.width
  }
  if (_curBasicClass & matClsEnum.SECTION_STEEL.V) {
    form[currentBasicClass.value][searchIdx.value].quantity = quantity
    form[currentBasicClass.value][searchIdx.value].length = data.length
    form[currentBasicClass.value][searchIdx.value].canUseQuantity = data.quantity
  }
  if (_curBasicClass & matClsEnum.STEEL_COIL.V) {
    form[currentBasicClass.value][searchIdx.value].quantity = quantity
    form[currentBasicClass.value][searchIdx.value].canUseQuantity = data.quantity
  }
  form[currentBasicClass.value][searchIdx.value].color = data.color
  form[currentBasicClass.value][searchIdx.value].brand = data.brand
  form[currentBasicClass.value][searchIdx.value].requisitionMode = requisitionModeEnum.USE_INVENTORY.V
  form[currentBasicClass.value][searchIdx.value].materialInventoryId = data.id
}

// 初始化
init()

FORM.HOOK.beforeToEdit = async (crud, form) => {
  // 修改的情况下，数据预处理
  await steelInboundFormFormat(form)
  // 设置监听等
  setFormCallback(form)
}

// 提交后清除校验结果
FORM.HOOK.afterSubmit = () => {
  emit('success')
  init()
}

// 表单校验
function validate() {
  if (form.type !== preparationTypeEnum.PUBLIC.V && isBlank(form.projectId)) {
    ElMessage.warning('请选择项目')
    return false
  }
  if (isBlank(form.steelPlateList) && isBlank(form.sectionSteelList) && isBlank(form.steelCoilList)) {
    ElMessage.warning('请填写数据')
    return false
  }
  const tableValidateRes = validateTable()
  if (tableValidateRes) {
    form.list = [...form.steelPlateList, ...form.sectionSteelList, ...form.steelCoilList]
    form.list.forEach((v) => {
      v.mete = v.weighingTotalWeight
      v.weight = v.weighingTotalWeight
    })
  }
  // 进入汇总页面
  return tableValidateRes
}

// 表格校验
function validateTable() {
  return Object.keys(steelRefList).every((k) => (steelRefList[k] ? steelRefList[k].validate() : true))
}

// 行数据添加时初始化
function rowInit(row) {
  return steelRef.value.rowInit(row)
}

// 信息初始化
function init() {
  nextTick(() => {
    steelRefList.steelPlateList = null
    steelRefList.sectionSteelList = null
    steelRefList.steelCoilList = null
    currentBasicClass.value = steelBasicClassKV.steelPlateList.K // 当前分类
    if (matSpecRef.value) {
      matSpecRef.value.clear()
    }
  })
}

// 解析导入表格
function handleExcelSuccess(list) {
  // 解析
  // 根据物料种类获取
  try {
    cu.props.import(list)
  } catch (error) {
    ElMessage.error({ message: error.message, duration: 5000 })
  }
}

// 导入
cu.props.import = (importList) => {
  const key = currentBasicClass.value
  // 先监听，后加入数组会导致监听失效
  // importList.forEach((v) => steelRefList[key].rowWatch(v))
  // 截取新旧数组长度，对导入数据进行rowWatch监听
  const oldLen = form[key].length
  form[key].push.apply(form[key], importList)
  const newLen = form[key].length
  for (let i = oldLen; i < newLen; i++) {
    steelRefList[key].rowWatch(form[key][i])
  }
  // 初始化选中数据，执行一次后取消当前监听
  const initSelectedTrigger = watch(
    matSpecRef,
    () => {
      if (matSpecRef.value) {
        matSpecRef.value.initSelected(
          importList.map((v) => {
            return { sn: v.sn, classifyId: v.classifyId }
          })
        )
        nextTick(() => {
          initSelectedTrigger()
        })
      }
    },
    { immediate: true }
  )
}
</script>

<style lang="scss" scoped>
.steel-requisitions-application-container {
  position: relative;
  .header {
    padding: 20px 20px 10px 20px;
  }
  .footer {
    position: absolute;
    bottom: 0;
    left: 0;
  }
}
</style>
