<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader class="head-container"/>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%"
    >
    <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
    <el-table-column v-if="columns.visible('monomerName')" key="monomerName" prop="monomerName" show-overflow-tooltip label="单体" align="center"/>
    <el-table-column v-if="columns.visible('quantity')" key="quantity" prop="quantity" show-overflow-tooltip :label="`${deepenTypeEnum.V?.[crud.query.productType]?.L}总数`" align="center"/>
    <el-table-column v-if="columns.visible('drawingQuantity')" key="drawingQuantity" prop="drawingQuantity" show-overflow-tooltip label="文件匹配总数" align="center"/>
    <!--编辑与删除-->
    <el-table-column
      v-if="checkPermission([...permission.import, ...permission.detail])"
      label="操作"
      width="170px"
      align="center"
      fixed="right"
    >
      <template v-slot="scope">
        <div class="btn">
          <upload-btn
            v-permission="permission.import"
            :dataType="scope.row.dataType"
            :accept="`.zip,${uploadType}`"
            :data="getParams(scope.row)"
            :tip="uploadType"
            size="mini"
            type="primary"
            @success="crud.toQuery"
          />
          <common-button v-permission="permission.detail" type="primary" size="mini" icon="el-icon-document" @click="openDetail(scope.row)" />
        </div>
      </template>
    </el-table-column>
  </common-table>
  <!--分页组件-->
  <pagination />
  <detail v-model="detailVisible" :currentRow="currentRow" :permission="permission" :upload-type="uploadType" :tip="`覆盖导入仅支持${tip}格式`" @success="crud.toQuery"/>
  </div>
</template>

<script setup>
import crudApi from '@/api/plan/technical-data-manage/technical-achievement'
import { ref, watch, computed } from 'vue'
import { mapGetters } from '@/store/lib'

import { cncFileListPM as permission } from '@/page-permission/plan'
import { deepenTypeEnum } from '@enum-ms/plan'
import checkPermission from '@/utils/system/check-permission'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import uploadBtn from '@/views/plan/technical-data-manage/technical-achievement/components/drawing-upload-btn.vue'
import detail from '@/views/plan/technical-data-manage/technical-achievement/components/common-detail.vue'

const { globalProjectId } = mapGetters(['globalProjectId'])

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const currentRow = ref({})
const detailVisible = ref(false)

const uploadType = computed(() => crud.query.productType === deepenTypeEnum.ASSEMBLY.V ? '.nc1' : '.dxf')
const tip = computed(() => crud.query.productType === deepenTypeEnum.ASSEMBLY.V ? 'NC1' : 'DXF')

const { crud, columns } = useCRUD(
  {
    title: '数控文件管理',
    permission: { ...permission },
    optShow: { ...optShow },
    requiredQuery: ['projectId', 'dataType'],
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight()

watch(
  () => globalProjectId.value,
  (val) => {
    if (val) {
      crud.query.projectId = globalProjectId.value
      crud.toQuery()
    }
  },
  { immediate: true, deep: true }
)

// 获取参数
function getParams(row) {
  return {
    projectId: row.projectId,
    monomerId: row.monomerId,
    productType: row.productType,
    dataType: row.dataType
  }
}

// 查看详情
function openDetail(row) {
  detailVisible.value = true
  currentRow.value = row
}
</script>

<style lang="scss" scoped>
::v-deep(.el-table) {
  .btn {
    *:nth-child(2){
      margin-left: 6px;
    }
  }
}
</style>
