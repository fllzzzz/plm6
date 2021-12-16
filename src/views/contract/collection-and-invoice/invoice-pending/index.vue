<template>
  <div class="app-container">
    <!--工具栏-->
    <div class="head-container">
      <mHeader ref="header" :permission="permission" />
    </div>
    <!--表格渲染 TODO: extra 可优化, 当该页面不以90%抽屉展示时，extra不应为100-->
    <el-table
      ref="table"
      v-loading="crud.loading"
      :border="$TBS.BORDER"
      :stripe="$TBS.STRIPE"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="$_tableMaxHeight({extra:100})"
      style="width: 100%;"
      @sort-change="crud.handleSortChange"
      @selection-change="crud.selectionChangeHandler"
    >
      <el-table-column v-if="$TBS.INDEX" :label="$TBS.INDEX_LABEL ? '序号': ''" type="index" align="center" width="60" />
      <el-table-column v-if="columns.visible('invoiceCollector')" key="invoiceCollector" prop="invoiceCollector" sortable="custom" :show-overflow-tooltip="true" label="收票单位" min-width="160" />
      <!-- <el-table-column v-if="columns.visible('invoiceNo')" key="invoiceNo" prop="invoiceNo" label="发票编号" align="left" min-width="140">
        <template v-slot="scope">
          <el-tag v-if="scope.row.oldInvoiceNo" type="danger" effect="plain" style="width:100%;max-width:140px">{{ scope.row.oldInvoiceNo | emptyTextFormatter }}</el-tag>
          <el-input
            v-else
            v-model="scope.row.invoiceNo"
            size="mini"
            placeholder="票号"
          />
        </template>
      </el-table-column> -->
      <el-table-column v-if="columns.visible('invoiceTypeAndTaxRate')" key="invoiceTypeAndTaxRate" prop="invoiceTypeAndTaxRate" :show-overflow-tooltip="true" label="发票类型" min-width="100" />
      <el-table-column v-if="columns.visible('invoiceAmount')" key="invoiceAmount" prop="invoiceAmount" sortable="custom" label="发票面额(元)" align="right" min-width="125">
        <template v-slot="scope">
          <el-tag type="success" effect="plain" style="width:100%;max-width:130px">{{ scope.row.invoiceAmount | toFixed($DP.YUAN) | toThousandFilter | emptyTextFormatter }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('tax')" key="tax" prop="tax" label="税额(元)" align="right" min-width="120">
        <template v-slot="scope">
          <el-tag type="warning" effect="plain" style="width:100%;max-width:120px">{{ scope.row.tax | toFixed($DP.YUAN) | toThousandFilter | emptyTextFormatter }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('invoiceDate')" key="invoiceDate" prop="invoiceDate" sortable="custom" label="开票日期" align="center" width="140">
        <template v-slot="scope">
          <span>{{ scope.row.invoiceDate | parseTime('{y}-{m}-{d}') }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('invoiceUnit')" key="invoiceUnit" prop="invoiceUnit" sortable="custom" :show-overflow-tooltip="true" label="开票单位" align="center" min-width="140" />
      <el-table-column v-if="columns.visible('applicantName')" key="applicantName" prop="applicantName" :show-overflow-tooltip="true" label="填报人" align="center" width="100px" />
      <el-table-column v-if="columns.visible('dataSourceSys')" key="dataSourceSys" prop="dataSourceSys" :show-overflow-tooltip="true" label="来源" align="center" width="100px">
        <template v-slot="scope">
          <span>{{ dataSourceSysEnumV[scope.row.dataSourceSys] && dataSourceSysEnumV[scope.row.dataSourceSys].L | emptyTextFormatter }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('remark')" key="remark" prop="remark" :show-overflow-tooltip="true" label="备注" min-width="160" />
      <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" sortable="custom" label="提交日期" align="center" width="160px">
        <template v-slot="scope">
          <span>{{ scope.row.createTime | parseTime }}</span>
        </template>
      </el-table-column>
      <!--   编辑与删除   -->
      <el-table-column
        v-if="checkPermission(permission.audit)"
        label="操作"
        width="130px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <pop-confirm-btn
            v-permission="permission.audit"
            :fn="handleInvoicing"
            :param="scope.row"
            :tip="`确认办理？`"
            :disabled="settlementStatusEnum.UNSETTLEMENT.V !== settlementStatus"
            placement="top"
            size="mini"
            type="primary"
            icon="el-icon-s-check"
            cancel-button-text="取消"
            @success="handleSuccess"
          />
        </template>
      </el-table-column>
    </el-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script>
import crudMethod, { handleInvoicing } from '@/api/contract/collection/invoice'
import mHeader from './module/header'
import CRUD, { presenter } from '@crud/crud'
import pagination from '@crud/Pagination'
import checkPermission from '@/utils/system/check-permission'
import enumOperate, { dataSourceSysEnum, settlementStatusEnum } from '@/utils/enum/index'
import popConfirmBtn from '@/views/components/common/pop-confirm-btn'
import { validatorInvoiceNo } from '@/utils/validatePattern'

const dataSourceSysEnumV = enumOperate.getVal(dataSourceSysEnum)

// crud交由presenter持有
const permission = {
  get: ['invoice:get'],
  audit: ['invoice:audit']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const crud = CRUD({
  title: '开票信息',
  sort: ['sort.asc', 'id.desc'],
  permission: { ...permission },
  crudMethod: { ...crudMethod },
  optShow: { ...optShow },
  requiredQuery: ['projectId']
})

export default {
  components: { mHeader, pagination, popConfirmBtn },
  mixins: [presenter(crud)],
  inject: ['$_tableMaxHeight'],
  props: {
    projectId: {
      type: [Number, String],
      default: undefined
    },
    settlementStatus: {
      type: Number,
      required: true
    }
  },
  data() {
    return {
      permission,
      dataSourceSysEnum,
      dataSourceSysEnumV,
      settlementStatusEnum
    }
  },
  watch: {
    projectId: {
      handler(newVal, oldVal) {
        this.crud.query.projectId = newVal
        this.crud.toQuery()
      }
    }
  },
  mounted() {
    this.crud.query.projectId = this.projectId
    this.crud.toQuery()
  },
  methods: {
    checkPermission,
    handle(row) {
      let msg
      if (!row.invoiceNo && row.invoiceNo.length > 20) {
        msg = '发票编号长度不可超过20字'
        this.$message.error(msg)
        throw new Error(msg)
      }
      if (!validatorInvoiceNo.test(row.invoiceNo)) {
        msg = '发票编号只能填写数字或字符'
        this.$message.error(msg)
        throw new Error(msg)
      }
      handleInvoicing({ id: row.id, invoiceNo: row.invoiceNo })
    },
    handleSuccess(val) {
      this.crud.toQuery()
      this.$emit('change')
    }
  }
}
</script>
