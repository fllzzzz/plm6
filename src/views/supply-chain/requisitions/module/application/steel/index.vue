<template>
  <div v-permission="permission" class="steel-requisitions-application-container">
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
            <el-radio-button v-for="item in steelBasicClassKV" :key="item.K" :label="item.K">
              {{ item.L }}{{ form[item.K] && form[item.K].length ? `(${form[item.K].length})` : '' }}
            </el-radio-button>
          </el-radio-group>
        </div>
        <div class="filter-right-box">
          <common-button class="filter-item" type="success" @click="materialSelectVisible = true">
            添加物料
          </common-button>
        </div>
      </div>
      <el-form ref="formRef" :model="form">
        <component ref="steelRef" :max-height="tableMaxHeight" :style="maxHeightStyle" :is="comp" />
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
  </div>
</template>

<script setup>
import crudApi from '@/api/supply-chain/requisitions-manage/requisitions'
import { steelInboundApplicationPM as permission } from '@/page-permission/wms'

import { defineProps, defineEmits, ref, computed, watch, provide, nextTick, reactive } from 'vue'
import { STEEL_ENUM } from '@/settings/config'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { preparationTypeEnum } from '@enum-ms/wms'

import useForm from '@/composables/form/use-form'
import useMaxHeight from '@compos/use-max-height'
import commonWrapper from './../components/common-wrapper.vue'
import materialTableSpecSelect from '@/components-system/classification/material-table-spec-select.vue'
import steelPlateTable from './module/steel-plate-table.vue'
import sectionSteelTable from './module/section-steel-table.vue'
import steelCoilTable from './module/steel-coil-table.vue'
import { ElMessage, ElRadioGroup } from 'element-plus'
import { isBlank, isNotBlank, toFixed } from '@/utils/data-type'

const emit = defineEmits(['success'])

const props = defineProps({
  detail: {
    type: Object
  }
})

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
const currentBasicClass = ref() // 当前基础分类
const list = ref([]) // 当前操作的表格list

// 钢材三个组件的ref列表
const steelRefList = reactive({
  steelPlateList: null,
  sectionSteelList: null,
  steelCoilList: null
})

provide('matSpecRef', matSpecRef) // 供兄弟组件调用 删除

const { form, FORM } = useForm(
  {
    title: '钢材申购',
    permission: permission,
    defaultForm: defaultForm,
    clearDraftCallback: init,
    api: crudApi.add
  },
  formRef,
  props.detail
)

watch(
  () => props.detail,
  (val = {}) => {
    form.type = val.type
    // 项目id
    if (val.type === preparationTypeEnum.PROJECT.V) {
      form.projectId = val.projectId
    } else if (val.type === preparationTypeEnum.PUBLIC.V) {
      form.projectId = []
    } else {
      // Array.isArray
      if (Array.isArray(val.projectId)) {
        form.projectId = []
      } else {
        form.projectId = val.projectId ? [val.projectId] : val.projectId
      }
    }
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
  extraBox: ['.el-drawer__header', '.filter-container', '.requisitions-application-header', '.requisitions-application-select', '.requisitions-application-footer'],
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
watch(list, (val) => {
  form[currentBasicClass.value] = val
})

// 初始化
init()

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
  // 进入仓库级价格填写页面
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

init()

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
