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
      <el-table-column v-if="columns.visible('name')" prop="name" key="name" label="名称" align="center" min-width="120" fixed="left" />
      <el-table-column
        v-if="columns.visible('serialNumber')"
        prop="serialNumber"
        key="serialNumber"
        label="编号"
        align="center"
        min-width="120"
        fixed="left"
      />
      <el-table-column v-if="columns.visible('quantity')" prop="quantity" key="quantity" label="清单数量" align="center" fixed="left" />
      <template v-for="item in process" :key="item.id">
        <el-table-column
          v-if="
            (crud.query.processType?.includes(item.productType) && componentTypeEnum.ARTIFACT.V) |
              (crud.query.processType?.includes(item.productType) && componentTypeEnum.ASSEMBLE.V)
          "
          :label="item.name"
          align="center"
          width="110px"
        >
          <template #default="{ row }">
            <div
              v-if="row.process[item.id] && row.process[item.id]?.inspectionQuantity === row.process[item.id]?.quantity"
              style="color: #13ce66"
            >
              √
            </div>
            <div v-else>
              <div v-if="row.process[item.id]">
                <span style="cursor: pointer" class="tc-danger">{{ row.process[item.id]?.inspectionQuantity }}</span>
                <span> / </span>
                <span>{{ row.process[item.id]?.quantity }}</span>
              </div>
              <span v-else> \ </span>
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
import { ref, provide, onMounted } from 'vue'
import crudApi from '@/api/mes/production-manage/dashboard/production-tracking'
import { getArtifactProcess } from '@/api/mes/production-config/artifact-rivet-weld-config'
import useCRUD from '@compos/use-crud'
// import { parseTime } from '@/utils/date'
import { arr2obj } from '@/utils/convert/type'
import { componentTypeEnum } from '@enum-ms/mes'
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
    // permission: { ...permission },
    crudApi: { ...crudApi },
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
const artifactTypeListObj = ref({})

provide('artifactTypeList', artifactTypeList)
provide('artifactTypeListObj', artifactTypeListObj)

async function fetchPreloadData() {
  try {
    const content = await getArtifactProcess()
    artifactTypeList.value = content.map((v) => {
      v.specPrefixStr = v.specPrefixList?.map((o) => o.specPrefix).join(' / ')
      const _obj = {}
      const _processIds = []
      if (v.productProcessLinkList) {
        for (const item of v.productProcessLinkList) {
          _obj[item['processId']] = {
            processLinkId: item.id,
            processId: item.processId
          }
          _processIds.push(item.processId)
        }
      }
      v.typeProcessObj = _obj
      v.processIds = _processIds
      return v
    })
    artifactTypeListObj.value = arr2obj(artifactTypeList.value, 'id')
  } catch (error) {
    console.log(error, '获取构件类型失败')
  }
}

onMounted(() => {
  fetchPreloadData()
})
CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.specPrefixStr = v.crossSectionPrefix?.join(' / ')
    v.needProcessIds = artifactTypeListObj.value[v.classificationId]?.processIds
    v.processObj =
      (v.structureProcessPriceList?.length && arr2obj(v.structureProcessPriceList, 'processId')) ||
      artifactTypeListObj.value[v.classificationId]?.typeProcessObj
    return v
  })
}
</script>
<style lang="scss" scoped>
</style>
