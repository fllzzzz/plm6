<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader />
    </div>
    <common-table
      ref="tableRef"
      v-loading="crud.loading || !loaded"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      row-key="projectId"
      style="width: 100%"
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" fixed="left" />
      <el-table-column
        v-if="columns.visible('name') && crud.query.processType === componentTypeEnum.ARTIFACT.V"
        prop="name"
        key="name"
        label="名称"
        align="center"
        min-width="120"
        fixed="left"
      />
      <el-table-column
        v-if="columns.visible('serialNumber')"
        prop="serialNumber"
        key="serialNumber"
        label="编号"
        align="center"
        fixed="left"
      />
      <el-table-column
        v-if="columns.visible('quantity')"
        prop="quantity"
        key="quantity"
        label="清单数量"
        align="center"
        fixed="left"
        width="100px"
      />
      <template v-for="item in process" :key="item.id">
        <el-table-column
          v-if="crud.query.processType === item.productType && item.productionLineTypeEnum & artifactProductLineEnum.TRADITION.V"
          :label="item.name"
          align="center"
        >
          <template #default="{ row }">
            <div
              v-if="
                row.processMap[item.id] &&
                row.processMap[item.id]?.inspectionQuantity !== 0 &&
                row.processMap[item.id]?.inspectionQuantity === row.processMap[item.id]?.quantity
              "
              style="color: #13ce66"
            >
              √
            </div>
            <div
              v-else-if="
                row.processMap[item.id] &&
                row.processMap[item.id]?.inspectionQuantity === 0 &&
                row.processMap[item.id]?.inspectionQuantity === row.processMap[item.id]?.quantity
              "
            >
              <span style="color: #303133">0</span>
              <span> / </span>
              <span style="color: #1682e6">0</span>
            </div>
            <div v-else-if="row.processMap[item.id] && row.processMap[item.id]?.inspectionQuantity !== row.processMap[item.id]?.quantity">
              <span style="cursor: pointer" class="tc-danger">{{ row.processMap[item.id]?.quantity }}</span>
              <span> / </span>
              <span>{{ row.processMap[item.id]?.inspectionQuantity }}</span>
            </div>
            <div v-else-if="!row.processMap[item.id]">
              <span> / </span>
            </div>
          </template>
        </el-table-column>
      </template>
    </common-table>
    <!-- 分页 -->
    <pagination />
  </div>
</template>
<script setup>
import { ref, provide, watch } from 'vue'
import { get as artifactTrack, assembleTrack, artifactAssembleList } from '@/api/mes/production-manage/dashboard/production-tracking'
import useCRUD from '@compos/use-crud'
import { mesProductionTrackingPM as permission } from '@/page-permission/mes'
import { componentTypeEnum, artifactProductLineEnum } from '@enum-ms/mes'
import useProcess from '@compos/store/use-process'
import useMaxHeight from '@compos/use-max-height'
import pagination from '@crud/Pagination'
import mHeader from './module/header.vue'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, CRUD, columns } = useCRUD(
  {
    title: '生产跟踪',
    sort: [],
    optShow: { ...optShow },
    permission: { ...permission },
    crudApi: { artifactTrack },
    requiredQuery: ['areaId'],
    hasPagination: true
  },
  tableRef
)
const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})

const { loaded, process } = useProcess()

const artifactTypeList = ref([])

provide('artifactTypeList', artifactTypeList)

async function fetchPreloadData() {
  if (!crud.query.areaId) return
  try {
    const { content } = await artifactAssembleList({
      areaId: crud.query.areaId,
      factoryId: crud.query.factoryId,
      taskTypeEnum: crud.query.processType
    })
    artifactTypeList.value = content || []
  } catch (error) {
    console.log(error, '获取构件类型失败')
  }
}

watch(
  [() => crud.query.areaId, () => crud.query.processType, () => crud.query.factoryId],
  (val) => {
    crud.query.classificationId = undefined
    fetchPreloadData()
  },
  { immediate: true }
)
CRUD.HOOK.beforeToQuery = async (crud) => {
  crud.crudApi.get = crud.query.processType === componentTypeEnum.ARTIFACT.V ? artifactTrack : assembleTrack
}
CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.processMap = {}
    v.process?.forEach((p) => {
      v.processMap[p.id] = p
    })
    return v
  })
}
</script>
<style lang="scss" scoped>
</style>
