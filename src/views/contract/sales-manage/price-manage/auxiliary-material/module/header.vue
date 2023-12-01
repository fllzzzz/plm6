<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <common-select
        v-model="query.useProperty"
        :options="auxiliaryMaterialUseTypeEnum.ENUM"
        type="enum"
        size="small"
        class="filter-item"
        clearable
        placeholder="使用范围"
      />
      <!-- <el-input
        v-model="query.name"
        placeholder="可输入名称搜索"
        class="filter-item"
        style="width: 200px;"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model="query.specification"
        placeholder="输入规格搜索"
        class="filter-item"
        style="width: 200px;"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model="query.serialNumber"
        placeholder="输入编号搜索"
        class="filter-item"
        style="width: 200px;"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      /> -->

      <rrOperation/>
    </div>
    <crudOperation>
      <template v-if="query.projectId" #optRight>
        <span v-if="checkPermission(crud.permission.save)" style="margin-right: 6px">
          <span v-if="modifying">
            <common-button type="warning" size="mini" @click="handelModifying(false, true)">取消录入</common-button>
            <common-button type="success" size="mini" @click="confirmModifying">预览并保存</common-button>
          </span>
          <common-button v-else type="primary" size="mini" :disabled="crud.selections?.length===0" @click="handelModifying(true)">录入价格</common-button>
        </span>
        <print-table
          v-permission="crud.permission.print"
          api-key="contractAuxiliaryMaterialPrice"
          :params="{ ...query }"
          size="mini"
          type="warning"
          class="filter-item"
        />
        <common-select
          type="enum"
          v-model="query.boolReturn"
          :options="whetherEnum.ENUM"
          clearable
          placeholder="是否退量"
          style="width: 200px"
          class="filter-item"
          @change="crud.toQuery"
        />
        <el-badge v-if="checkPermission(crud.permission.log) && priceEditMode===priceEditModeEnum.SAVE.V" :value="saveCount" :hidden="saveCount <= 0">
          <common-button type="success" size="mini" @click="handleLog">保存记录</common-button>
        </el-badge>
      </template>
      <!-- <template #viewLeft>
        <span v-if="checkPermission(crud.permission.cost) && query.projectId">
          <el-tag effect="plain" type="success" size="medium" class="filter-item">
            单体配套件总量：
            <span v-if="!costLoading">{{ monomerCost.totalMete }}</span>
            <i v-else class="el-icon-loading" />
          </el-tag>
          <el-tag effect="plain" type="success" size="medium">
            单体配套件造价(元)：
            <span v-if="!costLoading" v-thousand="monomerCost.totalPrice" />
            <i v-else class="el-icon-loading" />
          </el-tag>
        </span>
      </template> -->
    </crudOperation>
    <mPreview v-model="previewVisible" :modified-data="submitList" v-bind="$attrs" :params="previewParams" @success="handleSuccess" />
  </div>
</template>

<script setup>
// import { cost } from '@/api/contract/sales-manage/price-manage/auxiliary-material'
import { ref, watch, nextTick, inject, computed, defineExpose, defineProps, defineEmits } from 'vue'

import { auxiliaryMaterialUseTypeEnum } from '@enum-ms/plan'
import checkPermission from '@/utils/system/check-permission'
import { contractSaleTypeEnum } from '@enum-ms/mes'
import { priceEditModeEnum, standardPartPriceSearchEnum } from '@enum-ms/contract'
import { whetherEnum } from '@enum-ms/common'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import mPreview from '../../preview'

const projectId = inject('projectId')
const saveCount = inject('saveCount')
const priceEditMode = inject('priceEditMode')
const relationType = inject('relationType')
const emit = defineEmits(['checkSubmit', 'showVisible'])
const monomerId = inject('monomerId')
const areaId = inject('areaId')
const enclosurePlanId = inject('enclosurePlanId')
const category = inject('category')

const props = defineProps({
  showAble: {
    type: Boolean,
    default: false
  },
  submitList: {
    type: Array,
    default: () => []
  }
})

// 预览参数
const previewParams = computed(() => {
  switch (relationType) {
    case standardPartPriceSearchEnum.STRUCTURE.V:
      return {
        monomerId: query.monomerId,
        areaId: query.areaId,
        relationType: query.relationType,
        projectId: query.projectId,
        type: contractSaleTypeEnum.AUXILIARY_MATERIAL.V
      }
    default:
      return {
        category: query.category,
        enclosurePlanId: query.enclosurePlanId,
        relationType: query.relationType,
        projectId: query.projectId,
        type: contractSaleTypeEnum.AUXILIARY_MATERIAL.V
      }
  }
})

watch(
  projectId,
  (val) => {
    nextTick(() => {
      crud.query.projectId = val
      // costInit()
      crud.toQuery()
    })
  },
  { immediate: true }
)

watch(
  relationType,
  (val) => {
    nextTick(() => {
      crud.query.relationType = val
      if (relationType === standardPartPriceSearchEnum.STRUCTURE.V) {
        crud.query.monomerId = monomerId
        crud.query.areaId = areaId
      } else {
        crud.query.category = category
        crud.query.enclosurePlanId = enclosurePlanId
      }
      crud.toQuery()
    })
  },
  { immediate: true }
)

watch(
  enclosurePlanId,
  (val) => {
    nextTick(() => {
      crud.query.enclosurePlanId = val
      crud.toQuery()
    })
  },
  { immediate: true }
)

watch(
  monomerId,
  (val) => {
    nextTick(() => {
      crud.query.monomerId = val
      crud.toQuery()
    })
  },
  { immediate: true }
)

watch(
  areaId,
  (val) => {
    nextTick(() => {
      crud.query.areaId = val
      crud.toQuery()
    })
  },
  { immediate: true }
)

watch(
  category,
  (val) => {
    nextTick(() => {
      crud.query.category = val
      crud.toQuery()
    })
  },
  { immediate: true }
)

const modifying = ref(false)
const previewVisible = ref(false)
// const costLoading = ref(false)
// const costData = {
//   totalPrice: 0,
//   totalMete: 0
// }
// const monomerCost = ref({ ...costData })

const defaultQuery = {
  name: undefined, specification: undefined, serialNumber: undefined
}
const { crud, query, CRUD } = regHeader(defaultQuery)

// 刷新数据后
CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content.forEach((v, index) => {
    v.unitPrice = v.unitPrice || '同上'
    v.originUnitPrice = v.unitPrice
    v.totalPrice = v.mete * (v.unitPrice && typeof v.unitPrice === 'number' ? v.unitPrice : 0)
    v.orderIndex = index + 1
  })
}

function confirmModifying() {
  emit('checkSubmit')
  nextTick(() => {
    previewVisible.value = props.showAble
  })
}
// 获取商务配套件成本
// async function fetchCost() {
//   if (!checkPermission(crud.permission.cost)) return
//   costLoading.value = true
//   try {
//     const res = await cost({
//       projectId: projectId.value
//     })
//     monomerCost.value = res
//   } catch (error) {
//     console.log('获取商务配套件成本失败', error)
//   } finally {
//     costLoading.value = false
//   }
// }

// 成本初始化
// function costInit() {
//   monomerCost.value = { ...costData }
// }

// 处理录入状态
function handelModifying(status, reset = false) {
  // 取消分配，数据还原
  if (reset) {
    crud.data.forEach((v) => {
      v.unitPrice = v.originUnitPrice
      // v.newUnitPrice = v.originNewUnitPrice
      v.totalPrice = v.quantity * (v.unitPrice && typeof v.unitPrice === 'number' ? v.unitPrice : 0)
    })
  }
  modifying.value = status
}

// 提交成功后
function handleSuccess() {
  modifying.value = false
  crud.toQuery()
}

function handleLog() {
  emit('showVisible')
}

defineExpose({
  modifying
})
</script>
