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
      :max-height="maxHeight"
      style="width: 100%"
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column align="center" key="cutTaskName" prop="cutTaskName" :show-overflow-tooltip="true" label="工单名称" min-width="80">
        <template v-slot="scope">
          <span>{{ scope.row.cutTaskName }}</span>
        </template>
      </el-table-column>
      <el-table-column align="left" key="thick" prop="thick" :show-overflow-tooltip="true" label="厚度（mm）" min-width="50">
        <template v-slot="scope">
          <span>{{ scope.row.thick }}</span>
          <el-tag style="float: right; margin-right: 10px" v-if="scope.row.relationType && scope.row.relationType === 2" type="success">
            零件板
          </el-tag>
          <el-tag
            style="float: right; margin-right: 10px"
            v-else-if="scope.row.relationType && scope.row.relationType === 16"
            type="danger"
          >
            翼腹板
          </el-tag>
        </template>
      </el-table-column>
      <el-table-column align="center" key="material" prop="material" :show-overflow-tooltip="true" label="材质" min-width="40">
        <template v-slot="scope">
          <span>{{ scope.row.material }}</span>
        </template>
      </el-table-column>
      <el-table-column align="right" key="sum" prop="sum" :show-overflow-tooltip="true" label="零件数（件）" min-width="40">
        <template v-slot="scope">
          <span>{{ scope.row.sum }}</span>
        </template>
      </el-table-column>
      <el-table-column align="right" key="reduce" prop="reduce" :show-overflow-tooltip="true" label="零件量（kg）" min-width="40">
        <template v-slot="scope">
          <span>{{ scope.row.reduce }}</span>
        </template>
      </el-table-column>
      <el-table-column align="center" key="state" prop="state" :show-overflow-tooltip="true" label="状态" min-width="40">
        <template v-slot="scope">
          <span>
            <el-tag style="width: 100%" effect="plain" v-if="scope.row.state && scope.row.state === '1'" type="warning">
              套料指令已发送
            </el-tag>
            <el-tag style="width: 100%" effect="plain" v-else-if="scope.row.state && scope.row.state === '2'" type="success">
              套料结束
            </el-tag>
            <el-tag style="width: 100%" effect="plain" v-else-if="scope.row.state && scope.row.state === '0'" type="danger">
              未套料
            </el-tag>
          </span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('updateTime')" key="updateTime" prop="updateTime" label="编辑日期" width="140px">
        <template v-slot="scope">
          <span>{{ parseTime(scope.row.updateTime) }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" label="创建日期" width="140px">
        <template v-slot="scope">
          <span>{{ parseTime(scope.row.createTime) }}</span>
        </template>
      </el-table-column>
      <el-table-column width="250px" :show-overflow-tooltip="true" label="操作" align="center">
        <template v-slot="scope">
          <!-- <common-button v-if="scope.row.state === '0' || scope.row.state  === '1'" type="warning" size="mini">修改</common-button> -->
          <del-btn @query="crud.toQuery()" :data="scope.row" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import { ref } from 'vue'
import crudApi from '@/api/cutting/taskPack'
import useCRUD from '@compos/use-crud'
import { parseTime } from '@/utils/date'
import mHeader from './module/header'
import pagination from '@crud/Pagination'
import useMaxHeight from '@compos/use-max-height'
// import checkPermission from '@/utils/system/check-permission'
import delBtn from './module/del'

const tableRef = ref()

// crud交由presenter持有
const permission = {
  get: ['contractRecord:get'],
  edit: ['contractRecord:edit'],
  del: ['contractRecord:del']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const { crud, columns } = useCRUD(
  {
    title: '套料工单',
    sort: ['id.desc'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true,
    formStore: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.contractRecord',
  paginate: true,
  extraHeight: 40
})

// function NestingClick(row) {
//   console.log('row', row.cutTaskId)
// }
</script>
