<template>
  <div class="app-container">
    <!-- 工具栏 -->
    <mHeader />
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
      <!-- <el-table-column
        v-if="columns.visible('machineNumber')"
        align="center"
        key="machineNumber"
        prop="machineNumber"
        label="编号"
        min-width="40px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.machineNumber }}</span>
        </template>
      </el-table-column> -->
        <el-table-column
        v-if="columns.visible('factory')"
        align="center"
        key="factory"
        prop="factory"
        label="工厂"
        min-width="40px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.factory }}</span>
        </template>
      </el-table-column>
            <el-table-column v-if="columns.visible('workshopInf')" key="workshopInf" prop="workshopInf" label="车间" min-width="45px" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.workshopInf }}</span>
        </template>
      </el-table-column>
            <el-table-column v-if="columns.visible('productionLine')" key="productionLine" prop="productionLine" label="生产线" min-width="45px" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.productionLine }}</span>
        </template>
      </el-table-column>
         <el-table-column v-if="columns.visible('machineType')" key="machineType" prop="machineType" label="设备类型" min-width="45px" align="center">
        <template v-slot="scope">
          <span v-if="scope.row.machineType===machineTypeEnum.FLAME_CUTTING.V">火焰切割设备</span>
          <span v-if="scope.row.machineType===machineTypeEnum.PLASMA_CUTTING.V">等离子切割设备</span>
          <span v-if="scope.row.machineType===machineTypeEnum.LASER_CUTTING.V">激光切割设备</span>
          <!-- <span>{{MachineTypeEnum.VL[scope.row.machineType] }}</span> -->
        </template>
         </el-table-column>
            <el-table-column v-if="columns.visible('machineName')" key="machineName" prop="machineName" label="设备名称" min-width="40px" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.machineName }}</span>
        </template>
      </el-table-column>
            <el-table-column v-if="columns.visible('brand')" key="brand" prop="brand" label="设备品牌" min-width="40px" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.brand }}</span>
        </template>
      </el-table-column>
       <el-table-column
        v-if="columns.visible('machineNumber')"
        align="center"
        key="machineNumber"
        prop="machineNumber"
        label="设备编号"
        min-width="40px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.machineNumber }}</span>
        </template>
      </el-table-column>
       <el-table-column v-if="columns.visible('director')" key="director" prop="director" label="负责人" min-width="40px" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.director }}</span>
        </template>
      </el-table-column>
       <el-table-column v-if="columns.visible('mac')" key="mac" prop="mac" label="MAC地址" min-width="60px" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.mac }}</span>
        </template>
      </el-table-column>
       <el-table-column v-if="columns.visible('opcUrl')" key="opcUrl" prop="opcUrl" label="工控机地址" min-width="60px" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.opcUrl }}</span>
        </template>
      </el-table-column>
       <!-- <el-table-column v-if="columns.visible('machineName')" key="machineName" prop="machineName" label="机器名称" min-width="50px" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.machineName }}</span>
        </template>
      </el-table-column> -->
      <el-table-column
        v-if="columns.visible('online')"
        key="online"
        prop="online"
        align="center"
        label="链接状态"
        min-width="45px"
      >
        <template v-slot="scope">
          <span>{{ messageTypeEnum.VL[scope.row.online] }}</span>
        </template>
      </el-table-column>
      <!-- <el-table-column v-if="columns.visible('position')" key="position" prop="position" label="位置" min-width="45px" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.position }}</span>
        </template>
      </el-table-column>

      <el-table-column v-if="columns.visible('code')" key="code" prop="code" label="代号" min-width="40px" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.code }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('updateTime')" key="updateTime" prop="updateTime" label="编辑日期" width="140px" align="center">
        <template v-slot="scope">
          <span>{{ parseTime(scope.row.updateTime) }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" label="创建日期" width="140px" align="center">
        <template v-slot="scope">
          <span>{{ parseTime(scope.row.createTime) }}</span>
        </template>
      </el-table-column> -->
      <!-- 编辑与删除  -->
      <el-table-column v-if="checkPermission([...permission.edit])" label="操作" width="130px" align="center" fixed="right">
        <template v-slot="scope">
          <udOperation :showEdit="true" :showDel="false" :data="scope.row" />
          <!-- <udOperation  :data="scope.row" /> -->
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <!-- 表单 -->
    <m-form />
  </div>
</template>

<script setup>
import { ref } from 'vue'
import crudApi from '@/api/cutting/machine'
import useCRUD from '@compos/use-crud'
import mForm from './module/form'
// import { parseTime } from '@/utils/date'
import mHeader from './module/header'
import pagination from '@crud/Pagination'
import udOperation from '@crud/UD.operation'
import useMaxHeight from '@compos/use-max-height'
import checkPermission from '@/utils/system/check-permission'
import { messageTypeEnum, machineTypeEnum } from '@enum-ms/cutting'
import { machineConfigurationPM as permission } from '@/page-permission/cutting'

const tableRef = ref()

const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}

const { crud, columns } = useCRUD(
  {
    title: '机器配置',
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

</script>
