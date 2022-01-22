<template>
  <div class="app-container">
    <!--工具栏-->
    <div class="head-container">
      <mHeader :currentProjectType="currentProjectType"/>
    </div>
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
    <el-table-column v-if="columns.visible('serialNumber')" align="center" key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="合同编号" min-width="150">
      <template v-slot="scope">
        <span>{{ scope.row.serialNumber }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('name')" align="center" key="name" prop="name" :show-overflow-tooltip="true" label="项目名称" min-width="250">
      <template v-slot="scope">
        <span class="project-name">{{ scope.row.name }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('businessType')" key="businessType" prop="businessType" label="订单类型" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.businessType? businessTypeEnum.VL[scope.row.businessType]: '-' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('attachmentCount')" key="attachmentCount" prop="attachmentCount" label="文件份数" align="center" width="110px">
      <template v-slot="scope">
        <div>{{ scope.row.attachmentCount }}</div>
      </template>
    </el-table-column>
    <!--编辑与删除-->
    <el-table-column
      v-if="checkPermission([ ...permission.detail])"
      label="操作"
      width="130px"
      align="center"
      fixed="right"
    >
      <template v-slot="scope">
        <common-button type="primary" v-permission="permission.detail" @click="openDetail(scope.row)">查看</common-button>
      </template>
    </el-table-column>
  </common-table>
  <mDetail  v-model="detailVisible" />
  <!--分页组件-->
  <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/contract-record'
import { ref } from 'vue'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import { mapGetters } from '@/store/lib'
import mHeader from './module/header'
import { businessTypeEnum } from '@enum-ms/contract'
import checkPermission from '@/utils/system/check-permission'
import mDetail from './module/detail'
import { contractRecordPM as permission } from '@/page-permission/contract'

const { currentProjectType } = mapGetters(['currentProjectType'])

const optShow = {
  add: false,
  edit: false,
  del: true,
  download: false
}

const tableRef = ref()
const detailVisible = ref(false)
const { crud, columns } = useCRUD(
  {
    title: '合同档案',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.contractRecord',
  paginate: true,
  extraHeight: 40
})

function openDetail(row) {
  detailVisible.value = true
}
</script>

<style lang="scss" scoped>
::v-deep(.abnormal-row) {
  background: #e8f4ff;
}
::v-deep(.hidden-select) {
  td:nth-child(1){
    .cell{
      opacity:0;
    }
  }
}
$font-size: 1.5em;
.child {
  width: $font-size;
  height: $font-size;
  display: inline-block;
  border: 1px solid;
  border-radius: 50%;
  line-height: $font-size;
}
</style>
