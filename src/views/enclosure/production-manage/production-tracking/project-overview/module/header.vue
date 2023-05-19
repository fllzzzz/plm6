<template>
  <div>
    <div v-show="crud.searchToggle">
      <span class="filter-item">
        <el-checkbox v-model="checkAll" :indeterminate="isIndeterminate" :disabled="!areas.length" border @change="handleCheckAllChange">
          批次全选
        </el-checkbox>
      </span>
      <common-radio-button
        v-loading="categoryLoading"
        v-model="query.category"
        showOptionAll
        :options="categoryLoading ? [] : mesEnclosureTypeEnum.ENUM"
        type="enum"
        :unshowVal="unshowVal"
        class="filter-item"
        size="small"
      />
      <div class="area-wrap" v-loading="areaLoading">
        <span v-if="!areas.length">暂无批次</span>
        <el-checkbox-group v-model="query.planIds" @change="handleCheckedAresChange">
          <el-checkbox v-for="area in areas" :key="area.id" :label="area.id">
            {{ area.name }} / 交货期：<span v-parse-time="{ val: area.date, fmt: '{y}-{m}-{d}' }" />
          </el-checkbox>
        </el-checkbox-group>
      </div>
    </div>
    <crudOperation :show-grid="false" :show-refresh="false">
      <template #optRight>
        <color-card class="filter-item" v-model:value="query.status" :colors="colors" color-border select-able @change="crud.toQuery" />
      </template>
      <template #viewLeft>
        <scale v-model:value="boxScale" :intervals="400" @zoom-out="boxZoomOut" />
        <el-tag effect="plain" size="medium" style="margin-right: 6px">
          <span v-parse-project="{ project: props.project }" v-empty-text />
        </el-tag>
        <el-tag v-loading="summaryLoading" type="success" effect="plain" size="medium" style="margin-right: 6px">
          总量：<span v-thousand="summaryData?.totalLength || 0" /> m
        </el-tag>
        <el-tag v-loading="summaryLoading" type="success" effect="plain" size="medium" style="margin-right: 6px">
          已生产：<span v-thousand="summaryData?.completedLength || 0" /> m
        </el-tag>
        <el-tag v-loading="summaryLoading" type="success" effect="plain" size="medium">
          完成率：{{ summaryData?.completeRate || 0 }} %
        </el-tag>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { categoryList, areaList, enclosureSummary } from '@/api/enclosure/production-manage/project-overview'
import { ref, defineProps, computed, watch, nextTick, defineExpose, defineEmits } from 'vue'

import { mesEnclosureTypeEnum } from '@enum-ms/mes'
import { toFixed } from '@data-type/index'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import Scale from '@comp/Scale'
import ColorCard from '@comp/ColorCard'
import useDashboardHeader from '@compos/mes/dashboard/use-dashboard-header'

const emit = defineEmits('load')

const defaultQuery = {
  category: undefined,
  planIds: [],
  status: { value: undefined, resetAble: false }
}
const { crud, query, CRUD } = regHeader(defaultQuery)

const categoryArr = computed(() => {
  return Object.keys(mesEnclosureTypeEnum.V).map((v) => +v)
})

const props = defineProps({
  project: {
    type: Object,
    default: () => {}
  }
})

watch(
  () => props.project,
  (project) => {
    if (project?.id) {
      nextTick(() => {
        fetchCategory()
      })
    }
  },
  { immediate: true, deep: true }
)

watch(
  () => query.category,
  (val) => {
    nextTick(() => {
      fetchArea()
    })
  },
  { immediate: true }
)

const unshowVal = ref([]) // 不显示的围护种类
const categoryLoading = ref(false)
const areaLoading = ref(false)
const areas = ref([])
const checkAll = ref(false)
const isIndeterminate = ref(false)
const summaryLoading = ref(false)
const summaryData = ref({})

const boxScale = ref(1)

const { colors, boxZoomOut, getColor } = useDashboardHeader({ colorCardTitles: ['未生产', '生产中', '已完成'], emit, crud })

// 选择全部
const handleCheckAllChange = (val) => {
  query.planIds = val ? areas.value.map((v) => v.id) : []
  isIndeterminate.value = false
  crud.toQuery()
  fetchEnclosureSummary()
}

// 选择区域
const handleCheckedAresChange = (value) => {
  const checkedCount = value.length
  checkAll.value = checkedCount === areas.value.length
  isIndeterminate.value = checkedCount > 0 && checkedCount < areas.value.length
  crud.toQuery()
  fetchEnclosureSummary()
}

// 获取围护汇总
async function fetchEnclosureSummary() {
  try {
    summaryLoading.value = true
    summaryData.value = {
      totalLength: 0,
      completedLength: 0
    }
    if (query.planIds?.length) {
      summaryData.value = await enclosureSummary({
        projectId: props.project.id,
        planIds: query.planIds,
        category: query.category
      })
    }

    summaryData.value.completeRate =
      Number(summaryData.value.completedLength) && Number(summaryData.value.totalLength)
        ? toFixed((summaryData.value.completedLength / summaryData.value.totalLength) * 100, 2)
        : 0
  } catch (error) {
    console.log('获取获取围护汇总失败', error)
  } finally {
    summaryLoading.value = false
  }
}

// 获取围护种类
async function fetchCategory() {
  try {
    unshowVal.value = []
    query.category = undefined
    categoryLoading.value = true
    const arr = await categoryList({ projectId: props.project.id })
    unshowVal.value = categoryArr.value.filter((v) => !arr.includes(v))
    fetchArea()
    crud.toQuery()
  } catch (error) {
    console.log('获取所有项目下全部的围护种类失败')
  } finally {
    setTimeout(() => {
      categoryLoading.value = false
    }, 160)
  }
}

// 获取区域
async function fetchArea() {
  try {
    areas.value = []
    query.planIds = []
    if (!props.project?.id) return
    areaLoading.value = true
    areas.value = await areaList({ category: query.category, projectId: props.project.id })
    if (areas.value.length) {
      handleCheckAllChange(true)
      checkAll.value = true
    }
  } catch (error) {
    console.log('获取围护批次失败')
  } finally {
    areaLoading.value = false
  }
}

CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  data.content.forEach((row) => {
    row.boxColor = getColor(row, { quantity: 'producedQuantity', compare: 'quantity' })
  })
}

defineExpose({
  boxScale
})
</script>
<style lang="scss" scoped>
.area-wrap {
  height: 32px;
  margin-bottom: 10px;
  border-bottom: 1px solid #ebeef5;
  > span:first-child {
    line-height: 24px;
    font-size: 12px;
    margin: 0 10px;
    color: #409eff;
  }
  .el-checkbox-group {
    display: flex;
    overflow-x: auto;
  }
}

::-webkit-scrollbar {
  width: 6px;
  height: 6px;
}
::-webkit-scrollbar-thumb {
  border-radius: 6px;
}
</style>
