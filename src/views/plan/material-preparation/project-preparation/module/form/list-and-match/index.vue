<template>
  <div class="list-and-batch flex-rbs">
    <div class="list-container">
      <div class="filter-container">
        <div class="filter-left-box">
          <span class="table-title">清单汇总列表</span>
        </div>
        <div class="filter-right-box">
          <el-checkbox
            class="filter-item"
            v-model="queryFilter.boolPreparationLessThanList"
            label="只显示备料量小于清单量"
            size="mini"
            border
          />
        </div>
      </div>
      <component
        v-if="detail.technologyListType"
        v-loading="crud.editDetailLoading"
        :is="listComp"
        :list="filterList"
        :tech-prep-mete-k-v="crud.props.techPrepMeteKV"
        :height="props.height"
        :stripe="false"
        highlight-current-row
        @row-click="handleRowClick"
        style="width: 750px"
      />
    </div>
    <div class="match-container">
      <div class="match-table-wrapper">
        <match-table v-bind="$attrs" :height="props.height" :matchInfo="selectTechnologyRow" />
      </div>
    </div>
  </div>
</template>

<script setup>
import { ref, computed, defineEmits, defineProps } from 'vue'
import { componentTypeEnum } from '@enum-ms/building-steel'

import { regExtra } from '@compos/use-crud'
import StructureList from './structure'
import matchTable from './match-table.vue'
import { isBlank } from '@/utils/data-type'

const emit = defineEmits(['selected-change'])

const props = defineProps({
  height: {
    type: Number,
    default: 250
  }
})

// 当前物料
const selectTechnologyRow = ref()
// 查询过滤
const queryFilter = ref({
  boolPreparationLessThanList: false
})

// 技术清单汇总列表 过滤后的列表
const filterList = computed(() => {
  if (crud.form.technologyList) {
    return crud.form.technologyList.filter((row) => {
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

// 获取crud实例，并将实例注册进crud
const { CRUD, crud } = regExtra()
// 详情
const detail = crud.form

const listComp = computed(() => {
  switch (detail.technologyListType) {
    case componentTypeEnum.STRUCTURE.V:
      return StructureList // 构件技术清单
    case componentTypeEnum.ENCLOSURE.V:
      return StructureList // 围护技术清单
    case componentTypeEnum.AUXILIARY_MATERIAL.V:
      return StructureList // 辅材技术清单
    default:
      return null
  }
})

CRUD.HOOK.beforeToEdit = (crud, form) => {
  init()
}

CRUD.HOOK.beforeEditDetailLoaded = (crud, form) => {
  // 设置技术清单汇总
  // list.value = form.technologyList || []
}

// 初始化
function init() {
  selectTechnologyRow.value = undefined
}

// 行选中
function handleRowClick(row, column, event) {
  selectTechnologyRow.value = row
  emit('selected-change', row, column, event)
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
