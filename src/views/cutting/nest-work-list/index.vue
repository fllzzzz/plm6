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
      <el-table-column
        v-if="columns.visible('projectName')"
        key="projectName"
        prop="projectName"
        :show-overflow-tooltip="true"
        label="项目名称"
        min-width="130px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.projectName }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('plateNum')" align="center" key="plateNum" prop="plateNum" label="钢板总数" width="100px">
        <template v-slot="scope">
          <span>{{ scope.row.plateNum }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('distributionNum')"
        align="center"
        key="distributionNum"
        prop="distributionNum"
        label="排产数"
        width="100px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.distributionNum }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('plateWeight')"
        align="center"
        key="plateWeight"
        prop="plateWeight"
        label="钢板总重"
        width="100px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.plateWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('distributionWeight')"
        align="center"
        key="distributionWeight"
        prop="distributionWeight"
        label="排产量"
        width="100px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.distributionWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('partNum')" align="center" key="partNum" prop="partNum" label="零件数" width="100px">
        <template v-slot="scope">
          <span>{{ scope.row.partNum }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('distributionPartNum')"
        align="center"
        key="distributionPartNum"
        prop="distributionPartNum"
        label="已排零件数"
        width="100px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.distributionPartNum }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('partWeight')" align="center" key="partWeight" prop="partWeight" label="零件重" width="100px">
        <template v-slot="scope">
          <span>{{ scope.row.partWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('distributionPartWeight')"
        align="center"
        key="distributionPartWeight"
        prop="distributionPartWeight"
        label="已排零件重"
        width="100px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.distributionPartWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="checkPermission(permission.detail)" fixed="right" align="center" label="钢板清单" min-width="75px">
        <template v-slot="scope">
          <common-button icon="el-icon-view" type="primary" size="mini" @click="showDetail(scope.row)" />
        </template>
      </el-table-column>
      <el-table-column v-if="checkPermission(permission.Production)" fixed="right" align="center" label="操作" min-width="85px">
        <template v-slot="scope">
          <common-button @click="taskScheduling(scope.row)" type="success" size="mini">任务排产</common-button>
        </template>
      </el-table-column>
      <el-table-column v-if="checkPermission(permission.download)" fixed="right" align="center" label="套料成果" min-width="75px">
        <template v-slot="scope">
          <common-button
            :disabled="scope.row.reportUrl === null"
            icon="el-icon-download"
            @click="download(scope.row)"
            type="warning"
            size="mini"
          />
        </template>
      </el-table-column>
      <!-- <el-table-column align="center" :show-overflow-tooltip="true" label="钢板量（张 | kg）" min-width="80">
        <template v-slot="scope">
          <span class="quantity-mete-show tc-primary">
            <span class="left">{{ scope.row.plateNum }}</span>
            <span class="line">|</span>
            <span class="right">{{ scope.row.plateWeight }}</span>
          </span>
        </template>
      </el-table-column>
      <el-table-column align="center" :show-overflow-tooltip="true" label="排产数" min-width="80">
        <template v-slot="scope">
          <span class="quantity-mete-show tc-success">
            <span class="left">{{ scope.row.distributionNum }}</span>
            <span class="line">|</span>
            <span class="right">{{ scope.row.distributionWeight }}</span>
          </span>
        </template>
      </el-table-column>
      <el-table-column align="center" :show-overflow-tooltip="true" label="零件数" min-width="80">
        <template v-slot="scope">
          <span class="quantity-mete-show tc-primary">
            <span class="left">{{ scope.row.partNum }}</span>
            <span class="line">|</span>
            <span class="right">{{ scope.row.partWeight }}</span>
          </span>
        </template>
      </el-table-column>
      <el-table-column align="center" :show-overflow-tooltip="true" label="已排零件数" min-width="80">
        <template v-slot="scope">
          <span class="quantity-mete-show tc-success">
            <span class="left">{{ scope.row.distributionPartNum }}</span>
            <span class="line">|</span>
            <span class="right">{{ scope.row.distributionPartWeight }}</span>
          </span>
        </template>
      </el-table-column> -->
    </common-table>
    <!--分页组件-->
    <pagination />

    <!-- 钢板清单 -->
    <detail :detail-data="detailObj" v-model:visible="innerVisible" />
    <task-schedul :detail-data="detailObj" v-model:visible="quicklyAssignVisible"></task-schedul>
  </div>
</template>

<script setup>
import { ref } from 'vue'
import crudApi from '@/api/cutting/nestList'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import detail from '@/views/cutting/template/steel-plate-list.vue'
import taskSchedul from './module/task-scheduling'
import useMaxHeight from '@compos/use-max-height'
import checkPermission from '@/utils/system/check-permission'
import { ElNotification } from 'element-plus'
import { nestWorkListPM as permission } from '@/page-permission/cutting'

const tableRef = ref()
const innerVisible = ref(false)
const detailObj = ref()

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const { crud, columns } = useCRUD(
  {
    title: '套料工单',
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

const quicklyAssignVisible = ref(false) // 快速分配dlg

async function showDetail(row) {
  detailObj.value = row
  innerVisible.value = true
}

function download(row) {
  if (row.reportUrl !== null) {
    window.location.href = row.reportUrl
  } else {
    ElNotification({ title: '下载失败', message: '暂无套料成果 ', type: 'error' })
  }
}

function taskScheduling(row) {
  detailObj.value = row
  quicklyAssignVisible.value = true
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
