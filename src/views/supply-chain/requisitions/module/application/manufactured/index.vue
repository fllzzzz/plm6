<template>
  <div class="manuf-requisitions-application-container">
    <common-wrapper ref="commonWrapperRef" :show-footer="false">
      <div class="filter-container">
        <div class="filter-left-box">
          <div class="head-container" style="margin-bottom: 0px">
            <monomer-select-area-select
              v-if="isNotBlank(form.projectId)"
              v-model:monomerId="query.monomerId"
              v-model:areaId="query.areaId"
              :areaType="manufactureTypeEnum.OUTSOURCE.V"
              clearable
              areaClearable
              :project-id="form.projectId"
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
    <confirm-dialog v-model="previewVisible" is-manufactured @manuf-del="handleDel" />
  </div>
</template>

<script setup>
import crudApi from '@/api/supply-chain/requisitions-manage/requisitions'

import { defineProps, defineEmits, ref, computed, watch, provide, watchEffect, nextTick } from 'vue'
import { manufClsEnum } from '@/utils/enum/modules/classification'
import { manufactureTypeEnum } from '@enum-ms/plan'
import { isBlank, deepClone, toPrecision, isNotBlank } from '@/utils/data-type'
import { obj2arr } from '@/utils/convert/type'

import useForm from '@/composables/form/use-form'
import useMaxHeight from '@compos/use-max-height'
import commonWrapper from './../components/common-wrapper.vue'
import enclosureTable from './module/enclosure-table.vue'
import structureTable from './module/structure-table.vue'
import confirmDialog from '../components/confirm-dialog.vue'
import monomerSelectAreaSelect from '@comp-base/monomer-select-area-select'
import { ElMessage } from 'element-plus'

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

const defaultForm = {
  list: []
}

const commonWrapperRef = ref()
const tableRef = ref() // 表格ref
const formRef = ref() // form表单ref
const query = ref({})
const previewVisible = ref(false)
provide(
  'customQuery',
  computed(() => query.value)
)

const { form, FORM } = useForm(
  {
    title: '制成品申购',
    defaultForm: defaultForm,
    api: props.isEdit ? crudApi.edit : crudApi.add
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
    form.projectId = val.projectId
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

watchEffect(() => {
  form.list = obj2arr(form.purchaseListObj)
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

const totalBadge = computed(() => (isNotBlank(form.purchaseListObj) ? Object.keys(form.purchaseListObj).length : 0))

function handleAdd() {
  if (tableRef.value) {
    const list = tableRef.value.selectList
    if (!list?.length) {
      ElMessage.warning('请选择数据')
      return
    }
    for (let i = 0; i < list.length; i++) {
      const v = deepClone(list[i])
      const _purchaseWeight = v.curPurchaseQuantity * v.netWeight || 0
      if (isBlank(form.purchaseListObj[v.id])) {
        form.purchaseListObj[v.id] = {
          ...v,
          curPurchaseWeight: _purchaseWeight
        }
      } else {
        form.purchaseListObj[v.id].curPurchaseQuantity += v.curPurchaseQuantity
        form.purchaseListObj[v.id].curPurchaseWeight = toPrecision(form.purchaseListObj[v.id].curPurchaseWeight + _purchaseWeight, 2)
      }
    }
    refreshList()
  }
}

function handleDel({ id, quantity }, row) {
  const _purchaseWeight = quantity * row.netWeight || 0
  form.purchaseListObj[id].curPurchaseQuantity -= quantity
  form.purchaseListObj[id].curPurchaseWeight = toPrecision(form.purchaseListObj[id].curPurchaseWeight - _purchaseWeight, 2)
  if (form.purchaseListObj[id].curPurchaseQuantity === 0) {
    delete form.purchaseListObj[id]
  }
  refreshList()
}

function refreshList() {
  tableRef.value?.refreshList()
}

function refresh() {
  tableRef.value?.refresh()
}

watch(
  [() => form.projectId, () => query.value.monomerId, () => query.value.areaId],
  () => {
    query.value.projectId = form.projectId
    refresh()
  },
  { immediate: true }
)

function resetQuery() {
  query.value.monomerId = undefined
  query.value.areaId = undefined
  query.value.serialNumber = undefined
  refresh()
}

async function showPurchase() {
  let formValidate = true
  formValidate = await validate()
  previewVisible.value = formValidate
}

// 监听切换制成品类型，为list赋值
watch(
  () => form.finishedProductType,
  (val, oldVal) => {
    if (val && oldVal) {
      form.list = []
      form.purchaseListObj = {}
    }
  },
  { immediate: true }
)

FORM.HOOK.beforeToEdit = async (crud, form) => {
  form.purchaseListObj = {}
  const list = form.list
  for (let i = 0; i < list.length; i++) {
    const v = deepClone(list[i])
    const _purchaseWeight = v.quantity * v.netWeight || 0
    if (isBlank(form.purchaseListObj[v.id])) {
      form.purchaseListObj[v.id] = {
        ...v,
        curPurchaseQuantity: v.quantity,
        curPurchaseWeight: _purchaseWeight
      }
    } else {
      form.purchaseListObj[v.id].curPurchaseQuantity += v.quantity
      form.purchaseListObj[v.id].curPurchaseWeight = toPrecision(form.purchaseListObj[v.id].curPurchaseWeight + _purchaseWeight, 2)
    }
  }
  nextTick(() => {
    refresh()
  })
}

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
  return true
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
