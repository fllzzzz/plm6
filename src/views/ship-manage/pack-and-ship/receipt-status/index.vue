<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :data-format="dataFormat"
      style="width: 100%"
      :max-height="maxHeight"
      @sort-change="crud.handleSortChange"
      @selection-change="crud.selectionChangeHandler"
    >
      <el-table-column type="selection" width="50" align="center" />
      <el-table-column label="序号" type="index" align="center" width="50" />
      <el-table-column
        v-if="columns.visible('project.shortName') && !crud.query.projectId"
        key="project.shortName"
        prop="project.shortName"
        :show-overflow-tooltip="true"
        label="项目"
        min-width="250"
      >
        <template v-slot="scope">
          <span class="project-name">{{ projectNameFormatter(scope.row.project) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('manufactureType')"
        key="manufactureType"
        prop="manufactureType"
        :show-overflow-tooltip="true"
        label="制造类型"
        align="center"
        min-width="80"
      >
        <template v-slot="scope">
          <el-tag :type="manufactureTypeEnum.V[scope.row.manufactureType].T" effect="plain" disable-transitions>{{
            manufactureTypeEnum.VL[scope.row.manufactureType]
          }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('productType')" key="productType" prop="productType" label="装载类型" width="165">
        <template v-slot="scope">
          <el-tag
            v-for="item in cleanArray(EO.getBits(packTypeEnum, scope.row.productType, 'V'))"
            style="margin-right: 5px"
            :key="item"
            :type="packTypeEnum.V[item].T"
            effect="light"
            disable-transitions
            >{{ packTypeEnum.VL[item] }}</el-tag
          >
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('serialNumber')"
        key="serialNumber"
        prop="serialNumber"
        sortable="custom"
        label="车次"
        align="center"
        min-width="140px"
      >
        <template v-slot="scope">
          <el-tag effect="light" disable-transitions style="width: 100%; max-width: 130px">{{ scope.row.serialNumber }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('auditTime')"
        key="auditTime"
        prop="auditTime"
        align="center"
        sortable="custom"
        label="发运日期"
        width="120"
      >
        <template v-slot="scope">
          <span>{{ scope.row.auditTime }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('statusUpdateDate')"
        key="statusUpdateDate"
        prop="statusUpdateDate"
        sortable="custom"
        label="状态更新日期"
        align="center"
        width="120"
      >
        <template v-slot="scope">
          <span>{{ scope.row.statusUpdateDate }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('shipmentStatus')"
        key="shipmentStatus"
        prop="shipmentStatus"
        sortable="custom"
        label="发运状态"
        align="center"
        min-width="90"
      >
        <template v-slot="scope">
          <el-tag
            :type="deliveryReceiptStatusEnum.V[scope.row.shipmentStatus].T"
            disable-transitions
            effect="plain"
            v-if="scope.row.sourceRow.shipmentStatus"
            >{{ deliveryReceiptStatusEnum.VL[scope.row.shipmentStatus] }}</el-tag
          >
          <span v-else>-</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('licensePlate')"
        key="licensePlate"
        prop="licensePlate"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="车牌号"
        align="center"
        min-width="100"
      />
      <el-table-column
        v-if="columns.visible('driverName')"
        key="driverName"
        prop="driverName"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="司机姓名"
        align="center"
        min-width="100"
      />
      <el-table-column
        v-if="columns.visible('driverPhone')"
        key="driverPhone"
        prop="driverPhone"
        :show-overflow-tooltip="true"
        label="司机电话"
        align="center"
        min-width="120"
      />
      <el-table-column
        v-if="columns.visible('auditUserName')"
        key="auditUserName"
        prop="auditUserName"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="发运人"
        align="center"
        min-width="100"
      />
      <el-table-column
        v-if="columns.visible('receiptName')"
        key="receiptName"
        prop="receiptName"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="收货人"
        align="center"
        min-width="100"
      />
      <el-table-column
        v-if="columns.visible('auditReceiptName')"
        key="auditReceiptName"
        prop="auditReceiptName"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="实际收货人"
        align="center"
        min-width="100"
      />
      <el-table-column
        v-if="checkPermission([...permission.detail, ...permission.cancelDelivery, ...permission.confirmDelivery])"
        label="操作"
        width="180px"
        align="center"
        fixed="right"
      >
        <template #default="{ row: { sourceRow: row } }">
          <!-- 详情 -->
          <common-button
            type="primary"
            icon="el-icon-view"
            size="mini"
            v-if="checkPermission(permission.detail)"
            @click.stop="showDetail(row, 'detail')"
          />
          <common-button
            size="mini"
            type="success"
            v-if="
              row.shipmentStatus === deliveryReceiptStatusEnum.DELIVERY.V &&
              row.project?.businessType === businessTypeEnum.MACHINING.V &&
              checkPermission(permission.confirmDelivery)
            "
            @click.stop="showDetail(row, 'sign')"
            ><svg-icon
icon-class="sign"
          /></common-button>
          <common-button
            type="warning"
            size="mini"
            v-if="row.shipmentStatus === deliveryReceiptStatusEnum.DELIVERY.V && checkPermission(permission.cancelDelivery)"
            @click.stop="showDetail(row, 'cancel')"
            ><svg-icon
icon-class="delivery-cancel"
style="font-size: 14px"
          /></common-button>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <m-detail
      v-model:visible="detailVisible"
      :detail-info="receiptInfo"
      :title="showType === 'detail' ? '装车详情' : showType === 'cancel' ? '取消送货' : '到场签收'"
      :detailFunc="detail"
    >
      <template #tip>
        <div style="width: 150px; height: 53px; overflow: hidden; position: absolute; top: -18px; left: -20px">
          <table-cell-tag :show="receiptInfo.shipmentStatus === deliveryReceiptStatusEnum.RETURN.V" name="已取消" color="#f56c6c" />
        </div>
        <el-tag
          effect="plain"
          size="medium"
          v-if="receiptInfo.shipmentStatus"
          :type="deliveryReceiptStatusEnum.V[receiptInfo.shipmentStatus].T"
          >{{ deliveryReceiptStatusEnum.VL[receiptInfo.shipmentStatus] }}</el-tag
        >
        <el-tag effect="plain" size="medium" style="margin-left: 5px" type="danger">车次：{{ receiptInfo.serialNumber }}</el-tag>
        <el-tag effect="plain" size="medium">项目：{{ receiptInfo.project && receiptInfo.project.shortName }}</el-tag>
        <el-tag effect="plain" size="medium" type="success">发运人：{{ receiptInfo.auditUserName }}</el-tag>
        <el-tag effect="plain" size="medium" type="success">收货人：{{ receiptInfo.receiptName }}</el-tag>
      </template>
      <template #titleRight>
        <common-button
          type="warning"
          size="mini"
          v-if="showType === 'cancel' && checkPermission(permission.cancelDelivery)"
          @click.stop="cancelVisible = true"
          >取消送货</common-button
        >
        <el-popconfirm title="确定签收吗?" @confirm="signSubmit" v-if="showType === 'sign' && checkPermission(permission.confirmDelivery)">
          <template #reference>
            <common-button type="success" size="mini">确认签收</common-button>
          </template>
        </el-popconfirm>
      </template>
    </m-detail>
    <cancelForm
      v-model="cancelVisible"
      :detailInfo="detailInfo"
      @success="
        detailVisible = false,
        crud.toQuery()
      "
    />
  </div>
</template>

<script setup>
import { get, detail, deliverySign } from '@/api/ship-manage/pack-and-ship/receipt-status'
import { ref } from 'vue'

import { receiptStatusPM as permission } from '@/page-permission/ship-manage'
import { manufactureTypeEnum } from '@enum-ms/production'
import { businessTypeEnum } from '@enum-ms/contract'
import { packTypeEnum, deliveryReceiptStatusEnum } from '@enum-ms/mes'
import { projectNameFormatter } from '@/utils/project'
import { cleanArray } from '@/utils/data-type/array'
import EO from '@enum'
import checkPermission from '@/utils/system/check-permission'
import { ElNotification } from 'element-plus'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mDetail from '../components/common-detail'
import cancelForm from './module/cancel-form'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const dataFormat = [
  ['auditTime', ['parse-time', '{y}-{m}-{d}']],
  ['statusUpdateDate', ['parse-time', '{y}-{m}-{d}']]
]

const tableRef = ref()
const cancelVisible = ref(false)
const detailInfo = ref({})
const showType = ref('detail')
const { crud, columns } = useCRUD(
  {
    title: '收货状态',
    sort: ['auditReceiptTime.desc'],
    permission: { ...permission },
    crudApi: { get },
    optShow: { ...optShow },
    invisibleColumns: ['manufactureType', 'productType']
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

const detailVisible = ref(false)
const receiptInfo = ref({})

function showDetail(row, type) {
  showType.value = type
  receiptInfo.value = row
  detailInfo.value = row
  detailVisible.value = true
}

async function signSubmit() {
  try {
    await deliverySign(receiptInfo.value.id)
    ElNotification({ title: '签收成功', type: 'success' })
    detailVisible.value = false
    crud.toQuery()
  } catch (error) {
    console.log('签收失败', error)
  }
}

</script>
