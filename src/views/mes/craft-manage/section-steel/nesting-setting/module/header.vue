<template>
  <div v-show="crud.searchToggle">
    <el-form style="display: flex; flex-wrap: wrap">
      <el-form-item label="所属项目" class="form-label-require">
        <project-cascader
          v-model="query.projectId"
          placeholder="所属项目"
          clearable
          class="filter-item"
          style="width: 300px"
          @change="fetchOtherCondition"
        />
      </el-form-item>
      <el-form-item label="项目单体" class="form-label-require">
        <monomer-select-area-select
          v-model:monomerId="query.monomerId"
          v-model:areaId="query.ids"
          clearable
          areaClearable
          areaMultiple
          :project-id="query.projectId"
          @change="fetchOtherCondition"
        />
      </el-form-item>
      <el-form-item label="生产模式" class="form-label-require">
        <common-radio-button
          v-model="query.productionLineTypeEnum"
          :options="artifactProductLineEnum.ENUM"
          type="enum"
          default
          class="filter-item"
          @change="handleProductionLineTypeChange"
        />
        <common-radio-button
          v-if="!isTradition"
          v-model="query.boolMainAssemble"
          :options="assembleTypeEnum.ENUM"
          type="enum"
          class="filter-item"
          @change="crud.toQuery"
        />
      </el-form-item>
    </el-form>
    <el-form style="display: flex; flex-wrap: wrap">
      <el-form-item label="生产类型" class="form-label-require">
        <common-select
          v-model="query.structureClassId"
          :options="assembleList"
          :disabled="isTradition"
          type="other"
          :data-structure="{ key: 'id', label: 'label', value: 'id' }"
          class="filter-item"
          clearable
          style="width: 300px"
          placeholder="请选择生产类型"
          @change="handleAssembleChange"
        />
      </el-form-item>
      <el-form-item label="部件类型" class="form-label-require">
        <common-select
          v-model="query.specPrefix"
          :options="assemblePropertyList"
          type="other"
          :data-structure="{ key: 'specPrefix', label: 'label', value: 'specPrefix' }"
          class="filter-item"
          clearable
          style="width: 200px"
          placeholder="请选择部件类型"
          @change="handleAssemblePropertyChange"
        />
      </el-form-item>
      <el-form-item label="规格筛选">
        <common-select
          v-model="query.specification"
          :options="specificationList"
          type="other"
          clearable
          :data-structure="{ key: 'specification', label: 'specification', value: 'specification' }"
          class="filter-item"
          style="width: 200px"
          placeholder="请选择规格"
          @change="handleSpecificationChange"
        />
      </el-form-item>
      <el-form-item label="材质筛选">
        <common-select
          v-model="query.material"
          :options="materialList"
          type="other"
          clearable
          :data-structure="{ key: 'material', label: 'material', value: 'material' }"
          class="filter-item"
          style="width: 200px"
          placeholder="请选择材质"
          @change="crud.toQuery"
        />
      </el-form-item>
    </el-form>
  </div>
  <div v-show="crud.searchToggle">
    <el-tag effect="plain" size="medium" class="filter-item">长度≥：</el-tag>
    <el-input type="number" v-model.trim="query.length" class="filter-item" size="small" placeholder="输入长度搜索" style="width: 117px" />
    <el-input
      v-model.trim="query.serialNumber"
      size="small"
      placeholder="输入编号搜索"
      style="width: 200px"
      class="filter-item"
      clearable
      @keyup.enter="crud.toQuery"
    />
    <rrOperation />
  </div>
  <crudOperation>
    <template #optLeft>
      <common-radio-button
        v-if="isTradition"
        v-model="curMode"
        :options="[
          { type: 'nesting', label: '套料模式' },
          { type: 'edit', label: '编辑模式' },
        ]"
        :dataStructure="{ key: 'type', label: 'label', value: 'type' }"
        type="other"
        class="filter-item"
        @change="handleModeChange"
      />
      <common-button
        v-if="curMode === 'nesting'"
        class="filter-item"
        type="success"
        size="mini"
        icon="el-icon-menu"
        :disabled="crud.selections.length === 0"
        @click="handleExtrusionNesting"
      >
        型材套排
      </common-button>
      <common-button
        v-if="curMode === 'edit'"
        class="filter-item"
        type="warning"
        size="mini"
        icon="el-icon-setting"
        @click="handleNotNeedNesting"
      >
        移入【无需套料清单】
      </common-button>
    </template>
    <template #viewLeft>
      <slot name="viewLeft" />
      <common-button v-if="isTradition" class="filter-item" type="success" size="mini" icon="el-icon-view" @click="noNestingVisible = true">
        查看【无需套料清单】
      </common-button>
    </template>
  </crudOperation>
  <!-- <filter-drawer v-model:visible="filterVisible" :list="filterList"></filter-drawer> -->
  <no-nesting-drawer v-model:visible="noNestingVisible" @refresh="crud.toQuery" />
  <extrusion-nesting-setting v-model:visible="dialogVisible" :detail-data="crud.selections" :monomerId="query.monomerId" :projectId="query.projectId" />
</template>

<script setup>
import { getCondition, setNotNeedNesting } from '@/api/mes/craft-manage/section-steel/nesting-setting'
import { ref, computed, defineEmits, watchEffect } from 'vue'
import { ElMessage, ElNotification, ElMessageBox } from 'element-plus'

import { artifactProductLineEnum, assembleTypeEnum } from '@enum-ms/mes'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import monomerSelectAreaSelect from '@comp-base/monomer-select-area-select'
import noNestingDrawer from './no-nesting-drawer.vue'
import extrusionNestingSetting from './extrusion-nesting-setting.vue'

const emits = defineEmits(['change-mode'])

const defaultQuery = {
  projectId: { value: undefined, resetAble: false },
  monomerId: { value: undefined, resetAble: false },
  ids: { value: [], resetAble: false },
  structureClassId: undefined,
  specPrefix: undefined,
  boolMainAssemble: assembleTypeEnum.MAIN_ASSEMBLE.V
}

const { crud, query } = regHeader(defaultQuery)

const noNestingVisible = ref(false)
const dialogVisible = ref(false)
const detailData = ref({})
const curMode = ref('nesting')
const conditionInfo = ref({})
const assembleList = ref([])
const assemblePropertyList = ref([])
const specificationList = ref([])
const materialList = ref([])

const isTradition = computed(() => query.productionLineTypeEnum === artifactProductLineEnum.TRADITION.V)

watchEffect(() => {
  if (isTradition.value) {
    assembleList.value = []
    assemblePropertyList.value =
      (conditionInfo.value[artifactProductLineEnum.TRADITION.V] &&
        conditionInfo.value[artifactProductLineEnum.TRADITION.V][0]?.assemblePropertyDTOS?.map((v) => {
          v.label = `【${v.specPrefix}】${v.name}`
          return v
        })) ||
      []
  } else {
    assembleList.value =
      conditionInfo.value[artifactProductLineEnum.INTELLECT.V]?.map((v) => {
        v.label = `【${v.definitionWord}】${v.classificationName}`
        return v
      }) || []
    assemblePropertyList.value = []
  }
})

function handleProductionLineTypeChange(val) {
  query.structureClassId = undefined
  query.specPrefix = undefined
  query.specification = undefined
  query.material = undefined
  if (!isTradition.value) {
    curMode.value = 'nesting'
    handleModeChange(curMode.value)
  }
  fetchOtherCondition()
}

async function fetchOtherCondition() {
  if (!query.projectId || !query.monomerId) return
  try {
    conditionInfo.value = {}
    const data = await getCondition({
      projectId: query.projectId,
      monomerId: query.monomerId,
      ids: query.ids
    })
    conditionInfo.value = data
    crud.toQuery()
  } catch (er) {
    console.log(er, '获取其他筛选条件列表')
  }
}

function handleAssembleChange(val) {
  assemblePropertyList.value =
    assembleList.value
      .find((v) => v.id === val)
      ?.assemblePropertyDTOS?.map((v) => {
        v.label = `【${v.specPrefix}】${v.name}`
        return v
      }) || []
  crud.toQuery()
}

function handleAssemblePropertyChange(val) {
  specificationList.value = assemblePropertyList.value.find((v) => v.specPrefix === val)?.specificationList || []
  crud.toQuery()
}

function handleSpecificationChange(val) {
  materialList.value =
    specificationList.value
      .find((v) => v.specification === val)
      ?.material?.map((v) => {
        return {
          material: v
        }
      }) || []
  crud.toQuery()
}

function handleModeChange(mode) {
  emits('change-mode', mode)
}

async function handleNotNeedNesting() {
  if (!crud.selections?.length) {
    ElMessage.warning('请至少选择一条数据')
    return
  }

  try {
    ElMessageBox.confirm(`是否确认将所选的数据移入【无需套料清单】内`, '提示', {
      confirmButtonText: '确认',
      cancelButtonText: '取消',
      type: 'warning'
    }).then(async () => {
      try {
        const _data = crud.selections.map((v) => {
          return {
            // id: v.id,
            assembleDetailId: v.assembleDetailId,
            quantity: v.editQuantity
          }
        })
        await setNotNeedNesting(_data)
        ElNotification({
          title: '移入成功',
          type: 'success',
          duration: 2500
        })
        crud.toQuery()
      } catch (error) {
        console.log('无需套料设置失败', error)
      }
    })
  } catch (er) {
    console.log(er, '无需套料设置失败')
  }
}

// 型材套排
function handleExtrusionNesting(val) {
  dialogVisible.value = true
  detailData.value = val
}
</script>

<style lang="scss" scoped>
::v-deep(.el-form-item--small.el-form-item) {
  margin-bottom: 0;
  margin-right: 10px;
}
</style>
