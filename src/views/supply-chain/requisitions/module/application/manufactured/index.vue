<template>
  <div class="manuf-requisitions-application-container">
    <common-wrapper ref="commonWrapperRef" :show-footer="false">
      <div class="filter-container">
        <div class="filter-left-box">
          <div class="head-container" style="margin-bottom: 0px">
            <monomer-select-area-select
              v-if="isNotBlank(form.projectId?.[0])"
              v-model:monomerId="query.monomerId"
              v-model:areaId="query.areaId"
              :areaType="manufactureTypeEnum.OUTSOURCE.V"
              clearable
              areaClearable
              :project-id="form.projectId?.[0]"
            />
            <el-input
              v-model.trim="query.serialNumber"
              placeholder="输入编号搜索"
              class="filter-item"
              style="width: 200px"
              size="small"
              clearable
              @keyup.enter="refreshList"
            />
            <common-button class="filter-item" size="mini" type="success" icon="el-icon-search" @click.stop="refreshList">
              搜索
            </common-button>
            <common-button class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left" @click.stop="resetQuery">
              重置
            </common-button>
          </div>
        </div>
        <div class="filter-right-box">
          <common-button class="filter-item" size="mini" type="warning" icon="el-icon-plus" @click="handleAdd"> 选择加入 </common-button>
          <el-badge :value="totalBadge" :max="99" :hidden="totalBadge < 1" style="margin-right: 10px">
            <common-button class="filter-item" size="mini" type="success" icon="el-icon-shopping-cart-2" @click="showPurchase">
              采购车
            </common-button>
          </el-badge>
        </div>
      </div>
      <el-form ref="formRef" :model="form">
        <component ref="tableRef" :max-height="tableMaxHeight" :is="comp" />
      </el-form>
    </common-wrapper>
  </div>
</template>

<script setup>
import crudApi from '@/api/supply-chain/requisitions-manage/requisitions'

import { defineProps, defineEmits, ref, computed, watch, provide, nextTick } from 'vue'
import { manufClsEnum } from '@/utils/enum/modules/classification'
import { manufactureTypeEnum } from '@enum-ms/plan'

import useForm from '@/composables/form/use-form'
import useMaxHeight from '@compos/use-max-height'
import commonWrapper from './../components/common-wrapper.vue'
import enclosureTable from './module/enclosure-table.vue'
import structureTable from './module/structure-table.vue'
import { ElMessage } from 'element-plus'
import { isBlank, isNotBlank } from '@/utils/data-type'

import monomerSelectAreaSelect from '@comp-base/monomer-select-area-select'

const emit = defineEmits(['success'])

const props = defineProps({
  detail: {
    type: Object
  }
})

const defaultForm = {
  list: []
}

const commonWrapperRef = ref()
const tableRef = ref() // 表格ref
const formRef = ref() // form表单ref
const query = ref({})
provide(
  'customQuery',
  computed(() => query.value)
)

const { form, FORM } = useForm(
  {
    title: '制成品申购',
    defaultForm: defaultForm,
    api: crudApi.add
  },
  formRef,
  props.detail
)

watch(
  () => props.detail,
  (val = {}) => {
    form.purchaseListObj = {}
    form.type = val.type
    form.materialType = val.materialType
    form.finishedProductType = val.finishedProductType
    // 项目id
    if (Array.isArray(val.projectId)) {
      form.projectId = val.projectId
    } else {
      form.projectId = val.projectId ? [val.projectId] : []
    }
  },
  { deep: true, immediate: true }
)

const { maxHeight: tableMaxHeight } = useMaxHeight({
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
  paginate: true,
  navbar: false,
  minHeight: 300,
  extraHeight: 40
})

// 当前使用的组件
const comp = computed(() => {
  switch (form.finishedProductType) {
    case manufClsEnum.STRUC_MANUFACTURED.V:
      return structureTable
    case manufClsEnum.ENCL_MANUFACTURED.V:
      return enclosureTable
    default:
      return structureTable
  }
})

// // 总重
// const totalWeight = computed(() => {
//   let weight = 0
//   form.list.forEach((v) => {
//     weight += v.weighingTotalWeight ? v.weighingTotalWeight : 0
//   })
//   return toFixed(weight, 2)
// })

const totalBadge = computed(() => 0)

function handleAdd() {}

function refreshList() {
  nextTick(() => {
    tableRef.value.refresh()
  })
}

watch(
  [() => form.projectId, () => query.value.monomerId, () => query.value.areaId],
  () => {
    query.value.projectId = form.projectId
    refreshList()
  },
  { immediate: true }
)

function resetQuery() {
  query.value.monomerId = undefined
  query.value.areaId = undefined
  query.value.serialNumber = undefined
  refreshList()
}

function showPurchase() {}

// 监听切换制成品类型，为list赋值
watch(
  () => form.finishedProductType,
  (k) => {
    form.list = []
    form.purchaseListObj = {}
  },
  { immediate: true }
)

// 提交后清除校验结果
FORM.HOOK.afterSubmit = () => {
  emit('success')
}

// 表单校验
function validate() {
  if (isBlank(form.projectId)) {
    ElMessage.warning('请选择项目')
    return false
  }
  if (isBlank(form.list)) {
    ElMessage.warning('请选择数据')
    return false
  }
  const tableValidateRes = tableRef.value.validate()
  if (tableValidateRes) {
    form.list.forEach((v) => {
      v.mete = v.weighingTotalWeight
      v.weight = v.weighingTotalWeight
    })
  }
  // 进入汇总页面
  return tableValidateRes
}
</script>

<style lang="scss" scoped>
.manuf-requisitions-application-container {
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
