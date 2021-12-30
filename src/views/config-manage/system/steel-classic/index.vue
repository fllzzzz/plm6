<template>
  <div class="app-container">
    <!--工具栏-->
    <div class="head-container">
      <mHeader />
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
    <el-table-column v-if="columns.visible('name')" key="name" prop="name" :show-overflow-tooltip="true" label="钢材分类" min-width="150">
      <template v-slot="scope">
        <div>{{ scope.row.name }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('sort')" key="sort" prop="sort" :show-overflow-tooltip="true" label="排序" min-width="80">
      <template v-slot="scope">
        <span>{{ scope.row.sort }}</span>
      </template>
    </el-table-column>
    <el-table-column  v-if="columns.visible('links')" key="links" prop="links" label="关键字母【索引】" align="center" min-width="260">
      <template v-slot="scope">
        <template v-if="scope.row.links && scope.row.links.length>0">
          <span v-for="item in scope.row.links" :key="item.id">{{ `${item.keyword}【${item.specIndex}】`}}</span>
        </template>
      </template>
    </el-table-column>
    <!--编辑与删除-->
    <el-table-column
      v-if="checkPermission([...permission.del, ...permission.edit])"
      label="操作"
      width="130px"
      align="center"
      fixed="right"
    >
      <template v-slot="scope">
        <ud-operation :data="scope.row"/>
      </template>
    </el-table-column>
  </common-table>
  <mForm />
  </div>
</template>

<script setup>
import crudApi from '@/api/config/system-config/steel-classic'
import { ref } from 'vue'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import mHeader from './module/header'
import mForm from './module/form'

// crud交由presenter持有
const permission = {
  get: ['steelClassic:get'],
  add: ['steelClassic:add'],
  edit: ['steelClassic:edit'],
  del: ['steelClassic:del']
}

const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns } = useCRUD(
  {
    title: '钢材配置',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.collection',
  paginate: true,
  extraHeight: 40
})
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
