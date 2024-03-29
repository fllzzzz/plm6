<template>
  <div v-bind="$attrs" class="list-and-batch flex-rbs">
    <div class="list-container">
      <div class="filter-container">
        <div class="filter-left-box">
          <span class="table-title">清单汇总列表</span>
        </div>
        <div class="filter-right-box">
          <template v-if="crud.form.withoutList">
            <template v-if="!crud.props.boolTechEditMode">
              <div class="filter-item">
                <el-checkbox v-model="queryFilter.boolPreparationLessThanList" label="只显示备料量小于清单量" size="mini" border />
              </div>
              <common-button class="filter-item" type="success" size="mini" @click="techAddFormVisible = true"> 添 加 </common-button>
              <common-button class="filter-item" type="warning" size="mini" @click="toEditTech">编 辑</common-button>
            </template>
            <template v-else>
              <common-button class="filter-item" size="mini" type="primary" @click="saveTechEdit">保 存</common-button>
              <common-button class="filter-item" size="mini" type="danger" @click="cancelTechEdit">取 消</common-button>
            </template>
          </template>
        </div>
      </div>
      <component
        ref="technologyListRef"
        v-if="detail.technologyListType"
        v-loading="crud.editDetailLoading"
        :is="listComp"
        :list="filterList"
        :tech-prep-mete-k-v="techPrepMeteKV"
        :height="props.height"
        :stripe="false"
        :edit-mode="crud.props.boolTechEditMode"
        highlight-current-row
        @row-click="handleRowClick"
        :style="{ width: showMatchTable ? '750px' : '900px' }"
      />
    </div>
    <!-- 只有“结构”备料页面才足以显示匹配列表 -->
    <div class="match-container">
      <div class="match-table-wrapper">
        <match-table v-bind="$attrs" :height="props.height" :matchInfo="selectTechnologyRow" />
      </div>
    </div>
  </div>
  <tech-add-form v-model="techAddFormVisible" :technology-list="currentTechnologyList" @success="handleTechAddSuccess" />
</template>

<script setup>
import { ref, computed, defineEmits, defineProps, watch } from 'vue'
import { componentTypeEnum } from '@enum-ms/building-steel'

import { regExtra } from '@compos/use-crud'
import StructureList from './tech-list/structure'
import EnclosureList from './tech-list/enclosure'
import AuxMaterialList from './tech-list/aux-material'
import matchTable from './match-table.vue'
import techAddForm from './tech-add-form.vue'
import { isBlank, toPrecision } from '@/utils/data-type'
import { patternNumerical } from '@/utils/validate/pattern'
import cloneDeep from 'lodash/cloneDeep'
import { STEEL_BASE_UNIT } from '@/settings/config'

const emit = defineEmits(['selected-change'])

const props = defineProps({
  height: {
    type: Number,
    default: 250
  }
})

// 技术清单列表
const technologyListRef = ref()
// 当前物料
const selectTechnologyRow = ref()

// 备料清单添加显示
const techAddFormVisible = ref(false)

// 当前技术清单
const currentTechnologyList = ref()

// 编辑状态：清单
const editTechnologyList = ref()

// 编辑状态：备料量信息
const editTechPrepMeteKV = ref()

const showMatchTable = computed(() => crud.form.technologyListType === componentTypeEnum.STRUCTURE.V)

// 查询过滤
const queryFilter = ref({
  boolPreparationLessThanList: false
})
// 技术清单汇总列表 过滤后的列表
const filterList = computed(() => {
  if (crud.props.boolTechEditMode) {
    return editTechnologyList.value
  }
  if (currentTechnologyList.value) {
    return currentTechnologyList.value.filter((row) => {
      let meets = true
      if (queryFilter.value.boolPreparationLessThanList) {
        const info = crud.props.techPrepMeteKV[row.id]
        meets = isBlank(info) || !info.isEnough
      }
      return meets
    })
  } else {
    return []
  }
})

// 备料量信息
const techPrepMeteKV = computed(() => {
  if (crud.props.boolTechEditMode) {
    return editTechPrepMeteKV.value
  } else {
    return crud.props.techPrepMeteKV
  }
})

// 获取crud实例，并将实例注册进crud
const { CRUD, crud } = regExtra()

// 详情
const detail = crud.form

const listComp = computed(() => {
  switch (detail.technologyListType) {
    case componentTypeEnum.STRUCTURE.V:
      return StructureList // 构件技术清单
    case componentTypeEnum.ENCLOSURE.V:
      return EnclosureList // 围护技术清单
    case componentTypeEnum.AUXILIARY_MATERIAL.V:
      return AuxMaterialList // 辅材技术清单
    default:
      return null
  }
})

/**
 * 监听crud.form.technologyList变化，实现如下功能
 * 在清单编辑模式下，修改 editTechnologyList 中绑定的库存利用清单与需要采购清单信息
 */
watch(
  () => crud.form.technologyList,
  (list) => {
    if (Array.isArray(list)) {
      list.sort(techSort)
      currentTechnologyList.value = cloneDeep(crud.form.technologyList)

      // 编辑状态下
      if (crud.props.boolTechEditMode && editTechnologyList.value) {
        const kv = crud.props.technologyListKV
        /**
         * 此时需要采购清单中有数据被删除，才会触发
         * 因此以下代码没有问题，若是技术清单中的其他信息变更，可能需要重写此处代码。
         */
        editTechnologyList.value.forEach((item) => {
          // 当状态改变时，修改编辑清单中的信息
          item.boundPurIds = kv[item.id]?.boundPurIds
          item.boundInvIds = kv[item.id]?.boundInvIds
        })
      }
    }
  },
  {
    immediate: true,
    deep: true
  }
)

/**
 * 监听crud.props.techPrepMeteKV变化，实现如下功能
 * 在清单编辑模式下，修改 editTechPrepMeteKV 中的清单量
 */
watch(
  () => crud.props.techPrepMeteKV,
  (kv) => {
    // 编辑状态下
    if (crud.props.boolTechEditMode && typeof editTechPrepMeteKV.value === 'object') {
      const keys = Object.getOwnPropertyNames(editTechPrepMeteKV.value)
      keys.forEach(key => {
        const listMete = editTechPrepMeteKV.value[key].listMete
        editTechPrepMeteKV.value[key] = cloneDeep(kv[key])
        editTechPrepMeteKV.value[key].listMete = listMete
      })
    }
  },
  {
    deep: true
  }
)

CRUD.HOOK.beforeToEdit = (crud, form) => {
  init()
}

CRUD.HOOK.beforeEditDetailLoaded = (crud, form) => {
  // 设置技术清单汇总
  // list.value = form.technologyList || []
}

// 初始化
function init() {
  setCurrentRow()
  queryFilter.value.boolPreparationLessThanList = false
  editTechnologyList.value = undefined
  editTechPrepMeteKV.value = undefined
}

// 行选中
function handleRowClick(row) {
  // 修改模式不触发
  // if (!crud.props.boolTechEditMode) {
  selectTechnologyRow.value = row
  emit('selected-change', row)
  // }
}

// 处理添加成功
function handleTechAddSuccess(list) {
  // 按顺序插入
  list.forEach((techRow) => {
    const info = {}
    info.purchase = 0 // 库存利用量
    info.preparation = 0 // 总备料量
    info.diff = toPrecision(info.preparation, STEEL_BASE_UNIT.weight.precision) // 差值 = 总备料量 - 清单量
    info.isEnough = info.diff >= 0 // 是否超出
    crud.props.techPrepMeteKV[techRow.id] = info
  })
  // 重新设置crud.form.technologyList
  crud.form.technologyList = [...currentTechnologyList.value, ...list]
}

// 去修改清单
function toEditTech() {
  // technologyList.value 设置为table数据
  // technologyList.value = crud.form.technologyList
  // setCurrentRow()
  crud.props.boolTechEditMode = true
  editTechnologyList.value = cloneDeep(currentTechnologyList.value)
  editTechPrepMeteKV.value = cloneDeep(crud.props.techPrepMeteKV)
}

// 保存清单修改
function saveTechEdit() {
  crud.form.technologyList = cloneDeep(editTechnologyList.value)
  crud.props.techPrepMeteKV = cloneDeep(editTechPrepMeteKV.value)
  crud.props.boolTechEditMode = false
  // setCurrentRow()
}

// 取消编辑
function cancelTechEdit() {
  crud.props.boolTechEditMode = false
  // setCurrentRow()
}

// 清单汇总列表 排序
function techSort(a, b) {
  if (a.steelClassifyConfId === b.steelClassifyConfId) {
    if (a.material === b.material) {
      // 厚度比较
      if (patternNumerical.test(a.specification) && patternNumerical.test(b.specification)) {
        return Number(a.specification) < Number(b.specification) ? -1 : 1
      } else {
        return a.specification < b.specification ? -1 : 1
      }
    }
    return a.material < b.material ? -1 : 1
  }
  return a.steelClassifyConfId < b.steelClassifyConfId ? -1 : 1
}

// 设置当前行
function setCurrentRow(row) {
  selectTechnologyRow.value = row
  technologyListRef.value.setCurrentRow(row)
  emit('selected-change', row)
}
</script>

<style lang="scss" scoped>
::v-deep(.not-over-text) {
  color: #e6a23c;
}

::v-deep(.over-text) {
  color: #67c23a;
}
.list-and-batch {
  margin-top: -13px;
  width: 100%;
  .list-container {
    flex: none;
  }

  .match-container {
    height: 288px; // TODO:先写死，不设置高度会导致form的maxHeight计算错误，可能是一开始element中的表格高度并不是传入的值
    position: relative;
    flex: auto;
    margin-left: 20px;
    widows: 100%;

    .match-table-wrapper {
      width: 100%;
      position: absolute;
      right: 0;
      top: 0;
    }
  }
}

.table-title {
  background: dodgerblue;
  color: white;
  font-size: 14px;
  margin-top: 3px;
  margin-bottom: 10px;
  display: inline-block;
  padding: 3px 10px;
}
</style>
