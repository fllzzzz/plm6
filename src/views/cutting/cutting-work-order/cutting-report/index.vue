<template>
  <div class="app-container">
    <!-- 工具栏 -->
    <div class="head-container">
      <mHeader />
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight - 50"
      style="width: 100%"
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column key="workshopInf" align="center" prop="workshopInf" :show-overflow-tooltip="true" label="车间" min-width="40">
        <template v-slot="scope">
          <span>{{ scope.row.cutMachine.workshopInf }}</span>
        </template>
      </el-table-column>
      <el-table-column key="machineName" align="center" prop="machineName" :show-overflow-tooltip="true" label="设备名称" min-width="40">
        <template v-slot="scope">
          <el-tag style="width: 100%" effect="plain">
            <span>{{ scope.row.cutMachine.machineName }}</span>
          </el-tag>
        </template>
      </el-table-column>
      <el-table-column key="finishNum" align="center" prop="finishNum" :show-overflow-tooltip="true" label="完成数（件）" min-width="40">
        <template v-slot="scope">
          <span>{{ scope.row.finishNum ? scope.row.finishNum : 0 }}</span>
        </template>
      </el-table-column>
      <el-table-column
        key="finishWeight"
        align="center"
        prop="finishWeight"
        :show-overflow-tooltip="true"
        label="完成量（kg）"
        min-width="40"
      >
        <template v-slot="scope">
          <span>{{ scope.row.finishWeight ? scope.row.finishWeight : 0 }}</span>
        </template>
      </el-table-column>
      <el-table-column fixed="right" align="center" label="查看" min-width="80px">
        <template v-slot="scope">
          <common-button icon="el-icon-view" type="primary" size="mini" @click="showDetail(scope.row)" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />

    <detail :detail-data="detailObj" v-model:visible="innerVisible" />
  </div>
</template>

<script setup>
import crudApi from '@/api/cutting/machine-part'
import { ref } from 'vue'
import useCRUD from '@compos/use-crud'
import mHeader from './module/header'
import pagination from '@crud/Pagination'
import useMaxHeight from '@compos/use-max-height'
import detail from './module/detail/index.vue'

const tableRef = ref()
const innerVisible = ref(false)
const detailObj = ref()

// crud交由presenter持有
const permission = {
  get: ['contractRecord:get']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const { crud } = useCRUD(
  {
    title: '项目报表',
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

async function showDetail(row) {
  detailObj.value = row.cutMachine
  innerVisible.value = true
  console.log('  detailObj.value ', detailObj.value)
}

</script>

<style lang="scss">
.quantity-mete-show {
  display: flex;

  .left {
    width: 50%;
    text-align: right;
  }

  .right {
    width: 50%;
    text-align: left;
  }

  .line {
    width: 15px;
    text-align: center;
  }
}
</style>
