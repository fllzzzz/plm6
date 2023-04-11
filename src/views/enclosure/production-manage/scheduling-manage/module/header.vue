<template>
  <div>
    <div v-show="crud.searchToggle">
      <span class="filter-item">
        <el-checkbox v-model="checkAll" :indeterminate="isIndeterminate" border @change="handleCheckAllChange"> 批次全选 </el-checkbox>
      </span>
      <el-tag effect="plain" size="large" class="filter-item">{{ props.project?.name }}</el-tag>
      <common-radio-button
        v-loading="categoryLoading"
        v-model="query.category"
        :options="categoryLoading ? [] : mesEnclosureTypeEnum.ENUM"
        type="enum"
        :unshowVal="unshowVal"
        class="filter-item"
        size="small"
      />
      <div style="height: 32px; margin-bottom: 10px" v-loading="areaLoading">
        <el-checkbox-group v-model="query.planIds" @change="handleCheckedAresChange">
          <el-checkbox v-for="area in areas" :key="area.id" :label="area.id">
            {{ area.name }} / 交货期：<span v-parse-time="{ val: area.date, fmt: '{y}-{m}-{d}' }" />
          </el-checkbox>
        </el-checkbox-group>
      </div>
    </div>
    <crudOperation>
      <template #optLeft>
        <common-button v-permission="permission.add" :disabled="!crud.selections.length" type="primary" @click="dialogVisible = true">
          任务下发
        </common-button>
      </template>
      <template #viewLeft>
        <el-tag type="success" effect="plain" size="medium" style="margin-right: 6px">当前批次量：1000m</el-tag>
        <el-tag type="success" effect="plain" size="medium">全部批次量：1000m</el-tag>
      </template>
    </crudOperation>
    <common-dialog
      title="任务下发"
      v-model="dialogVisible"
      :before-close="
        () => {
          dialogVisible = false
        }
      "
      :close-on-click-modal="false"
      width="540px"
    >
      <template #titleRight>
        <common-button :loading="submitLoading" size="mini" type="primary" @click="submitIt"> 保存 </common-button>
      </template>
      <el-form ref="formRef" :model="taskForm" :rules="rules" size="small" label-width="170px">
        <el-form-item label="围护种类">
          <span>{{ mesEnclosureTypeEnum.VL?.[query.category] }}</span>
        </el-form-item>
        <el-form-item label="总量（m）">
          <span>{{ totalLength }}</span>
        </el-form-item>
        <el-form-item label="工厂/车间/生产线/班组" prop="teamId">
          <el-cascader
            v-model="taskForm.teamId"
            :options="groupsTree"
            :props="{
              value: 'id',
              label: 'name',
              children: 'children',
              expandTrigger: 'hover',
              emitPath: false,
            }"
            :show-all-levels="false"
            style="width: 300px"
            filterable
            clearable
            placeholder="请选择生产组"
          />
        </el-form-item>
        <el-form-item label="完成时间" prop="askCompleteTime">
          <el-date-picker v-model="taskForm.askCompleteTime" type="date" value-format="x" placeholder="选择完成时间" style="width: 300px" />
        </el-form-item>
      </el-form>
    </common-dialog>
  </div>
</template>

<script setup>
import { categoryList, areaList, add } from '@/api/enclosure/production-manage/scheduling-manage'
import { ref, defineProps, computed, watch, nextTick } from 'vue'
import { ElNotification } from 'element-plus'

import { mesEnclosureTypeEnum } from '@enum-ms/mes'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import useSchedulingGroups from '@compos/enclosure/scheduling/use-scheduling-groups'

const defaultQuery = {
  category: undefined,
  planIds: []
}
const { crud, query, CRUD } = regHeader(defaultQuery)

const queryParams = computed(() => {
  return {
    type: query.category
  }
})
const { groupsTree, groupsObj } = useSchedulingGroups({ queryParams })

const categoryArr = computed(() => {
  return Object.keys(mesEnclosureTypeEnum.V).map((v) => +v)
})

const totalLength = computed(() => {
  const num = crud.selections.reduce((total, cur) => {
    total += cur.totalLength
    return total
  }, 0)
  return num.toFixed(2)
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
const dialogVisible = ref(false)
const submitLoading = ref(false)
const taskForm = ref({})
const formRef = ref()

const rules = {
  teamId: [{ required: true, message: '请选择班组', trigger: 'change' }],
  askCompleteTime: [{ required: true, message: '请选择班组', trigger: 'change' }]
}

// 选择全部
const handleCheckAllChange = (val) => {
  query.planIds = val ? areas.value.map((v) => v.id) : []
  isIndeterminate.value = false
  crud.toQuery()
}

// 选择区域
const handleCheckedAresChange = (value) => {
  const checkedCount = value.length
  checkAll.value = checkedCount === areas.value.length
  isIndeterminate.value = checkedCount > 0 && checkedCount < areas.value.length
  crud.toQuery()
}

// 获取围护种类
async function fetchCategory() {
  try {
    unshowVal.value = []
    query.category = undefined
    categoryLoading.value = true
    const arr = await categoryList({ projectId: props.project.id })
    if (arr.length) {
      query.category = arr[0]
    }
    unshowVal.value = categoryArr.value.filter((v) => !arr.includes(v))
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
    if (!query.category || !props.project?.id) return
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

async function submitIt() {
  try {
    submitLoading.value = true
    const valid = await formRef.value.validate()
    if (!valid) return false
    const list = crud.selections.filter((v) => !!v.taskQuantity || typeof v.taskQuantity !== 'number')
    if (!list.length) {
      ElNotification({
        title: '没有任务数',
        type: 'error',
        duration: 2500
      })
      return
    }
    const group = groupsObj.value?.[taskForm.value.teamId] || {}
    await add({
      ...taskForm.value,
      category: query.category,
      projectId: props.project.id,
      factoryId: group.factory.id,
      workshopId: group.workshop.id,
      productionLineId: group.productionLine.id,
      schedulings: list.map((v) => {
        return {
          productId: v.id,
          quantity: v.taskQuantity
        }
      })
    })
    dialogVisible.value = false
    taskForm.value = {}
    ElNotification({
      title: '任务下发成功',
      type: 'success',
      duration: 2500
    })
    crud.toQuery()
  } catch (error) {
    console.log(error, '任务下发失败')
  } finally {
    submitLoading.value = false
  }
}

CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  data.content.forEach((row) => {
    row.totalLength = (row.length * row.needSchedulingQuantity) / 1000
    row.taskQuantity = row.needSchedulingQuantity
  })
}
</script>
<style lang="scss" scoped>
.el-checkbox-group {
  display: flex;
  overflow-x: auto;
  border-bottom: 1px solid #ebeef5;
}

::-webkit-scrollbar {
  width: 6px;
  height: 6px;
}
::-webkit-scrollbar-thumb {
  border-radius: 6px;
}
</style>
